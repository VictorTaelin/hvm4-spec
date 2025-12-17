// ((f ~> &{}) a)
// -------------- app-red-era
// &{}
fn Term wnf_app_red_era(void) {
  INTERACT("APP-RED-ERA");
  return term_new_era();
}
