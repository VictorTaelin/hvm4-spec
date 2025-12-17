// ! X &L = ^n
// ----------- dup-nam
// X₀ ← ^n
// X₁ ← ^n
fn Term wnf_dup_nam(u32 lab, u32 loc, u8 side, Term nam) {
  INTERACT("DUP-NAM");
  heap_subst_var(loc, nam);
  return nam;
}
