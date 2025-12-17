// (&{} a)
// ------- app-era
// &{}
fn Term wnf_app_era(void) {
  INTERACT("APP-ERA");
  return term_new_era();
}
