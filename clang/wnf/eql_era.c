// (&{} === b)
// ----------- eql-era-l
// &{}
fn Term wnf_eql_era_l(void) {
  INTERACT("EQL-ERA");
  return term_new_era();
}

// (a === &{})
// ----------- eql-era-r
// &{}
fn Term wnf_eql_era_r(void) {
  INTERACT("EQL-ERA");
  return term_new_era();
}
