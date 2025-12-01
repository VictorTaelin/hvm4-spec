fn Term snf(Term term, u32 depth) {
  term = wnf(term);
  u32 ari = term_arity(term);
  if (ari == 0) {
    return term;
  }
  u64 loc = term_val(term);
  if (term_tag(term) == LAM) {
    Term body = HEAP[loc];
    // #VAR{#depth{}} for stuck variable
    Term name_ctr = term_new_ctr(depth + 1, 0, NULL);
    heap_subst_var(loc, term_new_ctr(_VAR_, 1, (Term[]){name_ctr}));
    HEAP[loc] = snf(body, depth + 1);
  } else {
    for (u32 i = 0; i < ari; i++) {
      HEAP[loc + i] = snf(HEAP[loc + i], depth);
    }
  }
  return term;
}
