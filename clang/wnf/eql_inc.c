// (↑a === b)
// ----------- eql-inc-l
// ↑(a === b)
fn Term wnf_eql_inc_l(Term inc, Term b) {
  INTERACT("EQL-INC");
  u32  inc_loc = term_val(inc);
  Term a       = HEAP[inc_loc];
  Term eql     = term_new_eql(a, b);
  HEAP[inc_loc] = eql;
  return term_new(0, INC, 0, inc_loc);
}

// (a === ↑b)
// ----------- eql-inc-r
// ↑(a === b)
fn Term wnf_eql_inc_r(Term a, Term inc) {
  INTERACT("EQL-INC");
  u32  inc_loc = term_val(inc);
  Term b       = HEAP[inc_loc];
  Term eql     = term_new_eql(a, b);
  HEAP[inc_loc] = eql;
  return term_new(0, INC, 0, inc_loc);
}
