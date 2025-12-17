// ((f ~> ^n) a)
// ------------- app-red-nam
// ^((f ~> ^n) a)
fn Term wnf_app_red_nam(Term f, Term nam, Term arg) {
  INTERACT("APP-RED-NAM");
  return term_new_dry(term_new_red(f, nam), arg);
}
