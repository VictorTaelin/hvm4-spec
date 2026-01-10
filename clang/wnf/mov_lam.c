// % F = λx.f
// ---------- MOV-LAM
// F ← λx.G
// % G = f
fn Term wnf_mov_lam(u32 loc, Term lam) {
  ITRS++;
  u32  lam_loc = term_val(lam);
  u32  lam_ext = term_ext(lam);
  Term bod     = heap_read(lam_loc);

  u64 base  = heap_alloc(2);
  u32 g_loc = (u32)base;
  u32 x_loc = g_loc + 1;
  heap_write(g_loc, bod);
  heap_write(x_loc, term_new_got(g_loc));
  heap_subst_var(lam_loc, term_new(0, VAR, 0, x_loc));
  Term res = term_new(0, LAM, lam_ext, x_loc);
  if (!SAFE_MOV || (lam_ext & LAM_ERA_MASK)) {
    heap_subst_var(loc, res);
  }
  return res;
}
