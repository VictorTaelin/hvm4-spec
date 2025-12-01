fn Copy term_clone_at(u32 loc, u32 lab) {
  return (Copy){ term_new_co0(lab, loc), term_new_co1(lab, loc) };
}
