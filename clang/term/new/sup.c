fn Term term_new_sup_at(u32 loc, u32 lab, Term tm0, Term tm1) {
  return term_new_at(loc, SUP, lab, 2, (Term[]){tm0, tm1});
}

fn Term term_new_sup(u32 lab, Term tm0, Term tm1) {
  return term_new_sup_at(heap_alloc(2), lab, tm0, tm1);
}
