// @{s} n
// ------ alo-var
// s[n]
fn Term wnf_alo_var(u32 ls, u32 idx) {
  
  for (u32 i = 0; i < idx && ls != 0; i++) {
    ls = (u32)(HEAP[ls] & 0xFFFFFFFF);
  }
  u32 bind = (ls != 0) ? (u32)(HEAP[ls] >> 32) : 0;
  return bind ? term_new_var(bind) : term_new(0, VAR, 0, idx);
}
