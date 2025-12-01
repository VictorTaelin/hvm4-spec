fn Term term_new_app_at(u32 loc, Term fun, Term arg) {
  return term_new_at(loc, APP, 0, 2, (Term[]){fun, arg});
}

fn Term term_new_app(Term fun, Term arg) {
  return term_new_app_at(heap_alloc(2), fun, arg);
}
