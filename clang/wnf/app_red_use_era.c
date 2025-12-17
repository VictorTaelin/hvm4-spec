// ((f ~> λ{g}) &{})
// ----------------- app-red-use-era
// &{}
fn Term wnf_app_red_use_era(void) {
  INTERACT("APP-RED-USE-ERA");
  return term_new_era();
}
