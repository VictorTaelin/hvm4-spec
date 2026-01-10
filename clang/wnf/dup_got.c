// ! X &L = x
// --------- DUP-GOT
// ! A &L = v
// X₀ ← A₀
// X₁ ← A₁
fn Term wnf_dup_got(u32 lab, u32 loc, u8 side, Term got) {
  ITRS++;
  u32  got_loc = term_val(got);
  u32  got_ext = term_ext(got);
  Term val     = heap_read(got_loc);
  Copy V       = term_clone(lab, val);

  u64  loc0 = heap_alloc(1);
  u64  loc1 = heap_alloc(1);
  heap_write(loc0, V.k0);
  heap_write(loc1, V.k1);

  Term g0 = term_new(0, GOT, got_ext, (u32)loc0);
  Term g1 = term_new(0, GOT, got_ext, (u32)loc1);
  return heap_subst_cop(side, loc, g0, g1);
}
