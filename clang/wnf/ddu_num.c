// ! X &(#n) = v; b
// ---------------- ddu-num
// ! X &n = v
// b
fn Term wnf_ddu_num(Term lab_num, Term val, Term bod) {
  INTERACT("DDU-NUM");
  u32 lab      = term_val(lab_num);
  u64 dup_loc  = heap_alloc(2);
  Term co0     = term_new_co0(lab, dup_loc);
  Term co1     = term_new_co1(lab, dup_loc);
  Term app0    = term_new_app(bod, co0);
  Term app1    = term_new_app(app0, co1);
  HEAP[dup_loc + 0] = val;
  HEAP[dup_loc + 1] = app1;
  return term_new(0, DUP, lab, dup_loc);
}
