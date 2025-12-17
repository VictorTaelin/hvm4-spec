// ((f ~> mat) ↑x)
// --------------- app-red-mat-inc
// ↑((f ~> mat) x)
fn Term wnf_app_red_mat_inc(Term f, Term mat, Term inc) {
  INTERACT("APP-RED-MAT-INC");
  u32  inc_loc = term_val(inc);
  Term x       = HEAP[inc_loc];
  Term new_app = term_new_app(term_new_red(f, mat), x);
  return term_new_inc(new_app);
}
