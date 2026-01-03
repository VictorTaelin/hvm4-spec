// % X = T{a,b,...}
// ----------------- MOV-NOD
// % A = a
// % B = b
// ...
// X ← T{A,B,...}
fn Term wnf_mov_nod(u32 loc, Term term) {
  ITRS++;
  u32 ari = term_arity(term);
  if (ari == 0) {
    heap_subst_var(loc, term);
    return term;
  }
  u32  t_loc = term_val(term);
  u32  t_ext = term_ext(term);
  u8   t_tag = term_tag(term);
  // Aloca APENAS GOTs (não 2*ari!)
  u64  base    = heap_alloc((u64)ari);
  u32  got_loc = (u32)base;
  for (u32 i = 0; i < ari; i++) {
    // TAKE valor original
    Term arg = heap_take(t_loc + i);
    // Armazena no GOT
    heap_write(got_loc + i, arg);
    // Reescreve campo do node ORIGINAL
    heap_write(t_loc + i, term_new_got(got_loc + i));
  }
  // REUTILIZA o node original (t_loc, não nod_loc!)
  Term res = term_new(0, t_tag, t_ext, t_loc);
  heap_subst_var(loc, res);
  return res;
}