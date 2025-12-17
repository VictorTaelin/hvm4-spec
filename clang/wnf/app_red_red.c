// ((f ~> (g ~> h)) x)
// ------------------- app-red-red
// ((f x) ~> ((g ~> h) x))
fn Term wnf_app_red_red(Term f, Term red_inner, Term arg) {
  INTERACT("APP-RED-RED");
  return term_new_red(term_new_app(f, arg), term_new_app(red_inner, arg));
}
