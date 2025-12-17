// (x op &{}) where x is NUM
// -------------- op2-num-era
// &{}
fn Term wnf_op2_num_era() {
  INTERACT("OP2-NUM-ERA");
  return term_new_era();
}
