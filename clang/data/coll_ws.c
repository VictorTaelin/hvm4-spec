#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>

#ifndef COLL_WS_BRACKETS
#define COLL_WS_BRACKETS 64u
#endif

#ifndef COLL_WS_PRI_SHIFT
#define COLL_WS_PRI_SHIFT 0u
#endif

#ifndef COLL_WS_CAP_POW2
#define COLL_WS_CAP_POW2 21u
#endif

#ifndef COLL_WS_STEAL_ATTEMPTS
#define COLL_WS_STEAL_ATTEMPTS 2u
#endif

typedef struct __attribute__((aligned(256))) {
  WsDeque q[COLL_WS_BRACKETS];
  _Atomic u64 nonempty;
} CollWsBank;

typedef struct {
  CollWsBank bank[MAX_THREADS];
  u32 n;
} CollWs;

static inline u32 coll_ws_lsb64(u64 m) {
  return (u32)__builtin_ctzll(m);
}

static inline void coll_ws_mask_set(CollWs *ws, u32 tid, u32 b) {
  atomic_fetch_or_explicit(&ws->bank[tid].nonempty, (1ull << b), memory_order_relaxed);
}

static inline void coll_ws_mask_clear_owner(CollWs *ws, u32 tid, u32 b) {
  atomic_fetch_and_explicit(&ws->bank[tid].nonempty, ~(1ull << b), memory_order_relaxed);
}

static inline u8 coll_ws_pri_bucket(u32 pri) {
  u32 bucket = pri >> COLL_WS_PRI_SHIFT;
  if (bucket >= COLL_WS_BRACKETS) {
    return (u8)(COLL_WS_BRACKETS - 1u);
  }
  return (u8)bucket;
}

static inline bool coll_ws_init(CollWs *ws, u32 nthreads) {
  ws->n = nthreads;

  for (u32 t = 0; t < nthreads; ++t) {
    atomic_store_explicit(&ws->bank[t].nonempty, 0ull, memory_order_relaxed);
    for (u32 b = 0; b < COLL_WS_BRACKETS; ++b) {
      if (!wsq_init(&ws->bank[t].q[b], COLL_WS_CAP_POW2)) {
        for (u32 t2 = 0; t2 <= t; ++t2) {
          u32 bmax = COLL_WS_BRACKETS;
          if (t2 == t) {
            bmax = b;
          }
          for (u32 b2 = 0; b2 < bmax; ++b2) {
            wsq_free(&ws->bank[t2].q[b2]);
          }
        }
        return false;
      }
    }
  }
  return true;
}

static inline void coll_ws_free(CollWs *ws) {
  for (u32 t = 0; t < ws->n; ++t) {
    for (u32 b = 0; b < COLL_WS_BRACKETS; ++b) {
      wsq_free(&ws->bank[t].q[b]);
    }
  }
}

static inline void coll_ws_push(CollWs *ws, u32 tid, u8 pri, u64 task) {
  if (task == 0) {
    return;
  }
  u8 bucket = coll_ws_pri_bucket(pri);
  WsDeque *q = &ws->bank[tid].q[bucket];
  while (!wsq_push(q, task)) {
    cpu_relax();
  }
  coll_ws_mask_set(ws, tid, bucket);
}

static inline bool coll_ws_pop(CollWs *ws, u32 tid, u8 *pri, u64 *task) {
  u64 m = atomic_load_explicit(&ws->bank[tid].nonempty, memory_order_relaxed);
  if (m == 0ull) {
    return false;
  }
  while (m) {
    u32 b = coll_ws_lsb64(m);
    u64 x = 0;
    if (wsq_pop(&ws->bank[tid].q[b], &x)) {
      *pri = (u8)(b << COLL_WS_PRI_SHIFT);
      *task = x;
      return true;
    }
    coll_ws_mask_clear_owner(ws, tid, b);
    m &= (m - 1ull);
  }
  return false;
}

static inline u32 coll_ws_steal_some(
  CollWs *ws,
  u32     me,
  u32     max_batch,
  bool    restrict_deeper,
  u32    *cursor
) {
  u32 n = ws->n;
  if (n <= 1) {
    return 0u;
  }

  u64 my_mask = atomic_load_explicit(&ws->bank[me].nonempty, memory_order_relaxed);
  u32 my_min = COLL_WS_BRACKETS;
  if (my_mask != 0ull) {
    my_min = coll_ws_lsb64(my_mask);
  }

  u32 b_limit = COLL_WS_BRACKETS;
  if (restrict_deeper && my_min < b_limit) {
    b_limit = my_min;
  }

  u64 allowed_mask = ~0ull;
  if (b_limit < COLL_WS_BRACKETS) {
    allowed_mask = (1ull << b_limit) - 1ull;
  }

  u32 start = *cursor;
  for (u32 k = 0; k < COLL_WS_STEAL_ATTEMPTS; ++k) {
    u32 v = (start + k) % n;
    if (v == me) {
      continue;
    }
    u64 nm = atomic_load_explicit(&ws->bank[v].nonempty, memory_order_relaxed);
    nm &= allowed_mask;
    if (nm == 0ull) {
      continue;
    }
    u32 b = coll_ws_lsb64(nm);
    u32 got = 0;
    u64 x = 0;
    if (!wsq_steal(&ws->bank[v].q[b], &x)) {
      continue;
    }
    coll_ws_push(ws, me, (u8)(b << COLL_WS_PRI_SHIFT), x);
    got += 1u;
    for (; got < max_batch; ++got) {
      if (!wsq_steal(&ws->bank[v].q[b], &x)) {
        break;
      }
      coll_ws_push(ws, me, (u8)(b << COLL_WS_PRI_SHIFT), x);
    }
    *cursor = v + 1;
    return got;
  }
  *cursor = start + COLL_WS_STEAL_ATTEMPTS;
  return 0u;
}
