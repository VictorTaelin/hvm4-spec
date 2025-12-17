// &(↑x){a, b}
// ------------ dsu-inc
// ↑(&(x){a, b})
fn Term wnf_dsu_inc(Term inc, Term a, Term b) {
  INTERACT("DSU-INC");
  u32  inc_loc = term_val(inc);
  Term x       = HEAP[inc_loc];
  Term new_dsu = term_new_dsu(x, a, b);
  return term_new_inc(new_dsu);
}
