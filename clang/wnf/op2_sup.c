// @@opr(&L{a,b}, y)
// ------------------------- op2-sup
// ! Y &L = y
// &L{@@opr(a,Y₀), @@opr(b,Y₁)}
fn Term wnf_op2_sup(u32 opr, Term sup, Term y) {
  INTERACT("OP2-SUP");
  u32  lab     = term_ext(sup);
  u32  sup_loc = term_val(sup);
  Copy Y       = term_clone(lab, y);
  Term op0     = term_new_op2(opr, HEAP[sup_loc + 0], Y.k0);
  Term op1     = term_new_op2(opr, HEAP[sup_loc + 1], Y.k1);
  return term_new_sup(lab, op0, op1);
}
