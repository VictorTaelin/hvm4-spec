// Flatten
// =======
//
// Lazy collapse + extraction via priority queue traversal.
// INC nodes decrease priority (explored earlier), SUP nodes increase priority.
// Integrates collapse_step to handle infinite structures without stack overflow.

fn void collapse_flatten(Term term, int limit, int show_itrs) {
  // Priority queue for collapse ordering
  PQueue pq;
  pqueue_init(&pq);

  // Anchor the root term at a heap location
  u32 root_loc = heap_alloc(1);
  HEAP[root_loc] = term;
  pqueue_push(&pq, (PQItem){.pri = 0, .loc = root_loc});

  int count = 0;
  PQItem it;

  while (pqueue_pop(&pq, &it) && (limit < 0 || count < limit)) {
    u32 loc = it.loc;
    u8  pri = it.pri;

    // Lazy collapse: lift SUPs one step at a time
    Term t = collapse_step(HEAP[loc]);
    HEAP[loc] = t;

    // INC fast-path: peel chain of INC wrappers, decrementing priority
    while (term_tag(t) == INC) {
      u32 inc_loc = term_val(t);
      loc = inc_loc;
      t = collapse_step(HEAP[loc]);
      HEAP[loc] = t;
      if (pri > 0) pri--;  // decrement priority, clamped at 0
    }

    if (term_tag(t) == SUP) {
      // SUP at top - enqueue both branches with pri+1
      u32 sup_loc = term_val(t);
      pqueue_push(&pq, (PQItem){.pri = (u8)(pri + 1), .loc = sup_loc + 0});
      pqueue_push(&pq, (PQItem){.pri = (u8)(pri + 1), .loc = sup_loc + 1});
    } else if (term_tag(t) != ERA) {
      // Non-SUP, non-ERA result - normalize and print
      t = snf(t, 0, 1);  // quote=1: use quoted lambdas for collapser
      print_term(t);
      if (show_itrs) {
        printf(" \033[2m#%llu\033[0m", ITRS);
      }
      printf("\n");
      count++;
    }
  }

  pqueue_free(&pq);
}
