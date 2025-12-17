// (* === b)
// ----------- eql-any-l
// 1
fn Term wnf_eql_any_l(void) {
  INTERACT("EQL-ANY");
  return term_new_num(1);
}

// (a === *)
// ----------- eql-any-r
// 1
fn Term wnf_eql_any_r(void) {
  INTERACT("EQL-ANY");
  return term_new_num(1);
}
