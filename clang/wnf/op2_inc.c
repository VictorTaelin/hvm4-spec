// (↑x op y)
// ---------- op2-inc-x
// ↑(x op y)
fn Term wnf_op2_inc_x(u32 opr, Term inc, Term y) {
  INTERACT("OP2-INC");
  u32  inc_loc = term_val(inc);
  Term x       = HEAP[inc_loc];
  Term op      = term_new_op2(opr, x, y);
  HEAP[inc_loc] = op;
  return term_new(0, INC, 0, inc_loc);
}

// (#n op ↑y)
// ---------- op2-inc-y
// ↑(#n op y)
fn Term wnf_op2_inc_y(u32 opr, Term x, Term inc) {
  INTERACT("OP2-INC");
  u32  inc_loc = term_val(inc);
  Term y       = HEAP[inc_loc];
  Term op      = term_new_op2(opr, x, y);
  HEAP[inc_loc] = op;
  return term_new(0, INC, 0, inc_loc);
}
