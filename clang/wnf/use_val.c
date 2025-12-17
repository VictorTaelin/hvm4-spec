// (λ{f} x)
// --------- use-val
// (f x)
fn Term wnf_use_val(Term use, Term val) {
  INTERACT("USE-VAL");
  u32  loc = term_val(use);
  Term f   = HEAP[loc];
  return term_new_app(f, val);
}
