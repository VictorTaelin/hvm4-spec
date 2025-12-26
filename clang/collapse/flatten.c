// Flatten
// =======
//
// Lazy collapse + extraction via priority queue traversal.
// INC nodes decrease priority (explored earlier), SUP nodes increase priority.
// Integrates collapse_step to handle infinite structures without stack overflow.

#ifndef COLL_WS_BRACKETS
#define COLL_WS_BRACKETS 64u
#endif
#ifndef COLL_WS_STEAL_PERIOD
#define COLL_WS_STEAL_PERIOD 32u
#endif
#ifndef COLL_WS_STEAL_BATCH
#define COLL_WS_STEAL_BATCH 8u
#endif
#ifndef WSQ_L1
#define WSQ_L1 128
#endif

typedef struct __attribute__((aligned(256))) {
  WsDeque q[COLL_WS_BRACKETS];
  _Atomic u64 nonempty;
} CollBank;

typedef struct {
  _Alignas(WSQ_L1) _Atomic u64     printed;
  _Alignas(WSQ_L1) _Atomic int     stop;
  _Alignas(WSQ_L1) _Atomic int64_t pending;
  u64  limit;
  int  silent;
  int  show_itrs;
  u32  n;
  CollBank bank[MAX_THREADS];
} CollCtx;

typedef struct {
  CollCtx *ctx;
  u32 tid;
} CollArg;

static inline u32 coll_lsb64(u64 m) {
  return (u32)__builtin_ctzll(m);
}

static inline void coll_mask_set(CollCtx *C, u32 tid, u32 b) {
  atomic_fetch_or_explicit(&C->bank[tid].nonempty, (1ull << b), memory_order_relaxed);
}

static inline void coll_mask_clear_owner(CollCtx *C, u32 tid, u32 b) {
  atomic_fetch_and_explicit(&C->bank[tid].nonempty, ~(1ull << b), memory_order_relaxed);
}

static inline u8 coll_pri_clamp(u32 pri) {
  if (pri >= COLL_WS_BRACKETS) {
    return (u8)(COLL_WS_BRACKETS - 1u);
  }
  return (u8)pri;
}

static inline void coll_ws_push_local(CollCtx *C, u32 tid, u8 pri, u32 loc) {
  if (loc == 0) {
    return;
  }
  pri = coll_pri_clamp(pri);
  WsDeque *q = &C->bank[tid].q[pri];
  while (!wsq_push(q, (u64)loc)) {
    cpu_relax();
  }
  coll_mask_set(C, tid, pri);
}

static inline bool coll_ws_pop_local(CollCtx *C, u32 tid, u8 *pri, u32 *loc) {
  u64 m = atomic_load_explicit(&C->bank[tid].nonempty, memory_order_relaxed);
  if (m == 0ull) {
    return false;
  }
  while (m) {
    u32 b = coll_lsb64(m);
    u64 x = 0;
    if (wsq_pop(&C->bank[tid].q[b], &x)) {
      *pri = (u8)b;
      *loc = (u32)x;
      return true;
    }
    coll_mask_clear_owner(C, tid, b);
    m &= (m - 1ull);
  }
  return false;
}

static inline u32 coll_ws_steal_some(
  CollCtx *C,
  u32 me,
  u32 max_batch,
  bool restrict_deeper
) {
  u32 n = C->n;
  if (n <= 1) {
    return 0u;
  }

  u64 my_mask = atomic_load_explicit(&C->bank[me].nonempty, memory_order_relaxed);
  u32 my_min = (my_mask != 0ull) ? coll_lsb64(my_mask) : (u32)COLL_WS_BRACKETS;

  u32 b_limit = COLL_WS_BRACKETS;
  if (restrict_deeper && my_min < b_limit) {
    b_limit = my_min;
  }

  for (u32 b = 0; b < b_limit; ++b) {
    for (u32 v = 0; v < n; ++v) {
      if (v == me) {
        continue;
      }
      u64 nm = atomic_load_explicit(&C->bank[v].nonempty, memory_order_relaxed);
      if (((nm >> b) & 1ull) == 0ull) {
        continue;
      }
      u32 got = 0;
      u64 x = 0;
      if (!wsq_steal(&C->bank[v].q[b], &x)) {
        continue;
      }
      coll_ws_push_local(C, me, (u8)b, (u32)x);
      got += 1u;
      for (; got < max_batch; ++got) {
        if (!wsq_steal(&C->bank[v].q[b], &x)) {
          break;
        }
        coll_ws_push_local(C, me, (u8)b, (u32)x);
      }
      return got;
    }
  }
  return 0u;
}

static inline void coll_process_loc(CollCtx *C, u32 me, u8 pri, u32 loc, int64_t *pend_local) {
  for (;;) {
    if (atomic_load_explicit(&C->stop, memory_order_acquire)) {
      return;
    }

    Term t = collapse_step(heap_read(loc), 0);
    heap_set(loc, t);

    while (term_tag(t) == INC) {
      u32 inc_loc = term_val(t);
      loc = inc_loc;
      t = collapse_step(heap_read(loc), 0);
      heap_set(loc, t);
      if (pri > 0) {
        pri -= 1;
      }
    }

    if (term_tag(t) == SUP) {
      u32 sup_loc = term_val(t);
      u8 npri = (u8)(pri + 1);
      coll_ws_push_local(C, me, npri, sup_loc + 0);
      coll_ws_push_local(C, me, npri, sup_loc + 1);
      *pend_local += 2;
      return;
    }

    if (term_tag(t) == ERA) {
      return;
    }

    u64 prev = atomic_fetch_add_explicit(&C->printed, 1, memory_order_acq_rel);
    if (prev < C->limit) {
      if (!C->silent) {
        print_term_quoted(t);
        if (C->show_itrs) {
          printf(" \033[2m#%llu\033[0m", wnf_itrs_total());
        }
        printf("\n");
      }
    }

    if (prev + 1u >= C->limit) {
      atomic_store_explicit(&C->stop, 1, memory_order_release);
    }
    return;
  }
}

static void *collapse_flatten_worker(void *arg) {
  CollArg *A = (CollArg *)arg;
  CollCtx *C = A->ctx;
  u32 me = A->tid;

  wnf_set_tid(me);

  u32 iter = 0;
  u32 steal_period = COLL_WS_STEAL_PERIOD;
  u32 steal_batch = COLL_WS_STEAL_BATCH;
  int64_t pend_local = 0;

  for (;;) {
    if (atomic_load_explicit(&C->stop, memory_order_acquire)) {
      break;
    }

    u8 pri = 0;
    u32 loc = 0;
    bool has_local = coll_ws_pop_local(C, me, &pri, &loc);
    if (has_local) {
      coll_process_loc(C, me, pri, loc, &pend_local);
      pend_local -= 1;
      iter += 1u;
      if ((iter & (steal_period - 1u)) != 0u) {
        continue;
      }
    } else {
      iter = steal_period;
    }

    if (iter >= steal_period) {
      u32 got = coll_ws_steal_some(C, me, steal_batch, has_local);
      iter = 0;
      if (got > 0u) {
        continue;
      }
    }

    if (pend_local != 0) {
      atomic_fetch_add_explicit(&C->pending, pend_local, memory_order_release);
      pend_local = 0;
    }

    if (atomic_load_explicit(&C->pending, memory_order_acquire) == 0) {
      break;
    }

    cpu_relax();
  }

  if (pend_local != 0) {
    atomic_fetch_add_explicit(&C->pending, pend_local, memory_order_release);
  }

  wnf_itrs_flush(me);
  return NULL;
}

static inline u32 coll_ws_cap_pow2(u32 nthreads) {
  u32 threads = nthreads == 0 ? 1u : nthreads;
  u32 approx = (1u << 20) / threads;
  if (approx < (1u << 12)) {
    approx = (1u << 12);
  }

  u64 cap = (u64)approx;
  cap -= 1;
  cap |= cap >> 1;
  cap |= cap >> 2;
  cap |= cap >> 4;
  cap |= cap >> 8;
  cap |= cap >> 16;
  cap |= cap >> 32;
  cap += 1;

  u32 pow2 = 0;
  while (((u64)1 << pow2) < cap) {
    pow2 += 1;
  }
  return pow2;
}

static inline void collapse_flatten_seq(Term term, int limit, int show_itrs, int silent) {
  CollapseQueue pq;
  collapse_queue_init(&pq);

  u32 root_loc = heap_alloc(1);
  heap_set(root_loc, term);
  collapse_queue_push(&pq, (CollapseQueueItem){.pri = 0, .loc = root_loc});

  int count = 0;
  CollapseQueueItem it;

  while (collapse_queue_pop(&pq, &it) && (limit < 0 || count < limit)) {
    u32 loc = it.loc;
    u8  pri = it.pri;

    Term t = collapse_step(heap_read(loc), 0);
    heap_set(loc, t);

    while (term_tag(t) == INC) {
      u32 inc_loc = term_val(t);
      loc = inc_loc;
      t = collapse_step(heap_read(loc), 0);
      heap_set(loc, t);
      if (pri > 0) pri--;
    }

    if (term_tag(t) == SUP) {
      u32 sup_loc = term_val(t);
      collapse_queue_push(&pq, (CollapseQueueItem){.pri = (u8)(pri + 1), .loc = sup_loc + 0});
      collapse_queue_push(&pq, (CollapseQueueItem){.pri = (u8)(pri + 1), .loc = sup_loc + 1});
    } else if (term_tag(t) != ERA) {
      if (!silent) {
        print_term_quoted(t);
        if (show_itrs) {
          printf(" \033[2m#%llu\033[0m", wnf_itrs_total());
        }
        printf("\n");
      }
      count++;
    }
  }

  collapse_queue_free(&pq);
}

fn void collapse_flatten(Term term, int limit, int show_itrs, int silent) {
  u32 n = thread_get_count();
  if (n == 0) {
    n = 1;
  }
  if (n > MAX_THREADS) {
    n = MAX_THREADS;
  }
  if (n == 1) {
    collapse_flatten_seq(term, limit, show_itrs, silent);
    return;
  }
  if (limit == 0) {
    return;
  }

  u64 max_lines = limit < 0 ? UINT64_MAX : (u64)limit;

  u32 root_loc = heap_alloc(1);
  heap_set(root_loc, term);

  CollCtx C;
  atomic_store_explicit(&C.printed, 0, memory_order_relaxed);
  atomic_store_explicit(&C.stop, 0, memory_order_relaxed);
  atomic_store_explicit(&C.pending, 0, memory_order_relaxed);
  C.limit = max_lines;
  C.silent = silent;
  C.show_itrs = show_itrs;
  C.n = n;

  u32 cap_pow2 = coll_ws_cap_pow2(n);
  for (u32 t = 0; t < n; ++t) {
    atomic_store_explicit(&C.bank[t].nonempty, 0ull, memory_order_relaxed);
    for (u32 b = 0; b < COLL_WS_BRACKETS; ++b) {
      if (!wsq_init(&C.bank[t].q[b], cap_pow2)) {
        fprintf(stderr, "collapse: queue allocation failed\n");
        exit(1);
      }
    }
  }

  coll_ws_push_local(&C, 0u, 0u, root_loc);
  atomic_fetch_add_explicit(&C.pending, 1, memory_order_relaxed);

  pthread_t tids[MAX_THREADS];
  CollArg args[MAX_THREADS];
  for (u32 i = 1; i < n; ++i) {
    args[i].ctx = &C;
    args[i].tid = i;
    pthread_create(&tids[i], NULL, collapse_flatten_worker, &args[i]);
  }

  CollArg arg0 = { .ctx = &C, .tid = 0 };
  collapse_flatten_worker(&arg0);

  for (u32 i = 1; i < n; ++i) {
    pthread_join(tids[i], NULL);
  }

  for (u32 t = 0; t < n; ++t) {
    for (u32 b = 0; b < COLL_WS_BRACKETS; ++b) {
      wsq_free(&C.bank[t].q[b]);
    }
  }
}
