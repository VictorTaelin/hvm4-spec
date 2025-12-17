// @{s} n₀
// ------- alo-dp0
// s[n]₀
//
// @{s} n₁
// ------- alo-dp1
// s[n]₁
fn Term wnf_alo_cop(u32 ls, u32 idx, u32 lab, u8 side) {
  
  for (u32 i = 0; i < idx && ls != 0; i++) {
    ls = (u32)(HEAP[ls] & 0xFFFFFFFF);
  }
  u32 bind = (ls != 0) ? (u32)(HEAP[ls] >> 32) : 0;
  u8  tag  = side == 0 ? CO0 : CO1;
  return bind ? term_new(0, tag, lab, bind) : term_new(0, tag, lab, idx);
}
