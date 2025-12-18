// ($x === $x)  (same location)
// ---------------------------- eql-var
// #1
//
// ($x === $y)  (different location)
// --------------------------------- eql-var-miss
// #0
fn Term wnf_eql_var(Term a, Term b) {
  ITRS++;
  u32 a_loc = term_val(a);
  u32 b_loc = term_val(b);
  return term_new_num(a_loc == b_loc ? 1 : 0);
}
