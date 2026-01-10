// ! X &L = % Y = v; b
// -------------------- DUP-MOV
// ! A &L = v
// X₀ ← % Y0 = A₀; b[Y:=Y0]
// X₁ ← % Y1 = A₁; b[Y:=Y1]
fn Term mov_clone_rebind_got(Term t, u32 old_loc, u32 new_loc) {
  u8  tg  = term_tag(t);
  u32 vl  = term_val(t);
  u32 ext = term_ext(t);
  u8  sub = term_sub_get(t);

  if (tg == GOT && vl == old_loc) {
    Term v = term_new_got(new_loc);
    return sub ? term_sub_set(v, 1) : v;
  }

  u32 ari = term_arity(t);
  if (ari == 0) {
    return t;
  }

  u64 loc = heap_alloc(ari);
  for (u32 i = 0; i < ari; i++) {
    Term child = heap_read(vl + i);
    Term repl  = mov_clone_rebind_got(child, old_loc, new_loc);
    heap_write(loc + i, repl);
  }

  Term res = term_new(0, tg, ext, (u32)loc);
  return sub ? term_sub_set(res, 1) : res;
}

fn Term wnf_dup_mov(u32 lab, u32 loc, u8 side, Term mov) {
  ITRS++;
  u32  mov_loc = term_val(mov);
  u32  mov_ext = term_ext(mov);
  Term val     = heap_read(mov_loc + 0);
  Term bod     = heap_read(mov_loc + 1);

  Copy V = term_clone(lab, val);

  u64  loc0 = heap_alloc(2);
  u64  loc1 = heap_alloc(2);
  Term bod0 = mov_clone_rebind_got(bod, mov_loc, (u32)loc0);
  Term bod1 = mov_clone_rebind_got(bod, mov_loc, (u32)loc1);

  heap_write(loc0 + 0, V.k0);
  heap_write(loc0 + 1, bod0);
  heap_write(loc1 + 0, V.k1);
  heap_write(loc1 + 1, bod1);

  Term m0 = term_new(0, MOV, mov_ext, (u32)loc0);
  Term m1 = term_new(0, MOV, mov_ext, (u32)loc1);
  return heap_subst_cop(side, loc, m0, m1);
}
