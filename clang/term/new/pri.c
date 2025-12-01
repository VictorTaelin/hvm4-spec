fn Term term_new_pri_at(u32 loc, u32 nam, u32 ari, Term *args) {
  return term_new_at(loc, P00 + ari, nam, ari, args);
}

fn Term term_new_pri(u32 nam, u32 ari, Term *args) {
  return term_new_pri_at(heap_alloc(ari), nam, ari, args);
}
