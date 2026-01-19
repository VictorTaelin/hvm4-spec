fn Term term_new_pri_at(u32 loc, u32 prim, u32 ari, Term *args) {
  return term_new_at(loc, PRI, prim, ari, args);
}

fn Term term_new_pri(u32 prim, u32 ari, Term *args) {
  return term_new_pri_at(heap_alloc(ari), prim, ari, args);
}
