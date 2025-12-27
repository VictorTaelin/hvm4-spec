// data/wsq.c - Chase-Lev work-stealing deque for u64 tasks.
//
// Context
// - Used by parallel evaluators to distribute heap locations across workers.
// - Single-owner pushes and pops from the bottom; other threads steal from the top.
//
// Design
// - Ring buffer of fixed capacity (power of two) storing u64 tasks.
// - Atomic top/bottom indices are cache-line padded to limit false sharing.
// - Owner operations are wait-free except for full/empty checks.
// - Steals are lock-free and may fail under contention.
//
// Notes
// - Not multi-producer: only the owner thread may push/pop.
// - Capacity is fixed after init; wsq_push returns 0 when full.
// - Counters are monotonic; wrap-around is not guarded (practically unreachable).

#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

// Cache-line size used to pad indices and the buffer alignment.
#ifndef WSQ_L1
#define WSQ_L1 128
#endif

// Cache-line padded atomic index used by the deque.
typedef struct {
  _Atomic size_t v;
  char _pad[WSQ_L1 - sizeof(_Atomic size_t)];
} WsqIdx;

// Work-stealing deque state (single owner, multi-stealer).
typedef struct __attribute__((aligned(128))) {
  WsqIdx top;
  WsqIdx bot;
  _Alignas(WSQ_L1) u64 *buf;
  size_t mask;
  size_t cap;
} WsDeque;

// Allocate aligned memory for the ring buffer.
static inline void *wsq_aligned_alloc(size_t alignment, size_t nbytes) {
  void *ptr = NULL;
  size_t size = ((nbytes + alignment - 1) / alignment) * alignment;
  int err = posix_memalign(&ptr, alignment, size);
  if (err) {
    return NULL;
  }
  return ptr;
}

// Initialize a deque with 2^capacity_pow2 slots.
static inline int wsq_init(WsDeque *q, u32 capacity_pow2) {
  size_t cap = (size_t)1 << capacity_pow2;
  q->buf = (u64 *)wsq_aligned_alloc(WSQ_L1, cap * sizeof(u64));
  if (!q->buf) {
    return 0;
  }
  q->cap  = cap;
  q->mask = cap - 1;
  atomic_store_explicit(&q->top.v, 0, memory_order_relaxed);
  atomic_store_explicit(&q->bot.v, 0, memory_order_relaxed);
  return 1;
}

// Release the deque buffer.
static inline void wsq_free(WsDeque *q) {
  if (q && q->buf) {
    free(q->buf);
    q->buf = NULL;
  }
}

// Owner push to the bottom; returns 1 on success, 0 if full.
static inline int wsq_push(WsDeque *q, u64 x) {
  size_t b = atomic_load_explicit(&q->bot.v, memory_order_relaxed);
  size_t t = atomic_load_explicit(&q->top.v, memory_order_acquire);
  if (b - t >= q->cap) {
    return 0;
  }
  __builtin_prefetch(&q->buf[b & q->mask], 1, 1);
  q->buf[b & q->mask] = x;
  atomic_store_explicit(&q->bot.v, b + 1, memory_order_release);
  return 1;
}

// Owner pop from the bottom; returns 1 on success, 0 if empty or lost race.
static inline int wsq_pop(WsDeque *q, u64 *out) {
  size_t b = atomic_load_explicit(&q->bot.v, memory_order_relaxed);
  if (b == 0) {
    return 0;
  }
  size_t b1 = b - 1;
  __builtin_prefetch(&q->buf[b1 & q->mask], 0, 1);
  atomic_store_explicit(&q->bot.v, b1, memory_order_release);
  atomic_thread_fence(memory_order_acq_rel);

  size_t t = atomic_load_explicit(&q->top.v, memory_order_acquire);
  if (t <= b1) {
    u64 x = q->buf[b1 & q->mask];
    if (t == b1) {
      size_t expected = t;
      bool ok = atomic_compare_exchange_strong_explicit(
        &q->top.v,
        &expected,
        t + 1,
        memory_order_acq_rel,
        memory_order_acquire
      );
      if (!ok) {
        atomic_store_explicit(&q->bot.v, t + 1, memory_order_release);
        return 0;
      }
      atomic_store_explicit(&q->bot.v, t + 1, memory_order_release);
    }
    *out = x;
    return 1;
  } else {
    atomic_store_explicit(&q->bot.v, t, memory_order_release);
    return 0;
  }
}

// Thief steal from the top; returns 1 on success, 0 if empty or lost race.
static inline int wsq_steal(WsDeque *q, u64 *out) {
  size_t t = atomic_load_explicit(&q->top.v, memory_order_acquire);
  size_t b = atomic_load_explicit(&q->bot.v, memory_order_acquire);
  if (t >= b) {
    return 0;
  }
  __builtin_prefetch(&q->buf[t & q->mask], 0, 1);
  u64 x = q->buf[t & q->mask];
  size_t expected = t;
  bool ok = atomic_compare_exchange_strong_explicit(
    &q->top.v,
    &expected,
    t + 1,
    memory_order_acq_rel,
    memory_order_acquire
  );
  if (ok) {
    *out = x;
    return 1;
  }
  return 0;
}
