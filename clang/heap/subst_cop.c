fn Term heap_subst_cop(u8 side, u32 loc, Term r0, Term r1) {
  HEAP[loc] = term_mark(side == 0 ? r1 : r0);
  return side == 0 ? r0 : r1;
}
