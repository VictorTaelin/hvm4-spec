// ((f ~> ^(g x)) a)
// ----------------- app-red-dry
// ^((f ~> ^(g x)) a)
fn Term wnf_app_red_dry(Term f, Term dry, Term arg) {
  INTERACT("APP-RED-DRY");
  return term_new_dry(term_new_red(f, dry), arg);
}
