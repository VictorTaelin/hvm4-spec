fn Term term_new_lam_at(u32 loc, Term bod) {
  return term_new_at(loc, LAM, 0, 1, (Term[]){bod});
}

fn Term term_new_lam(Term bod) {
  return term_new_lam_at(heap_alloc(1), bod);
}
