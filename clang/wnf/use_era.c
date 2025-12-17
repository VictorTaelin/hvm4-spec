// (λ{f} &{})
// ---------- use-era
// &{}
fn Term wnf_use_era() {
  INTERACT("USE-ERA");
  return term_new_era();
}
