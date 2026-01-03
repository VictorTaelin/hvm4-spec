// % K = &L{x,y}
// ------------- MOV-SUP
// % A = x
// % B = y
// K ← &L{A,B}
fn Term wnf_mov_sup(u32 loc, Term sup) {
  ITRS++;
  u32  s_loc = term_val(sup);
  u32  s_lab = term_ext(sup);
  // Aloca APENAS 2 GOTs (não 4!)
  u64  base   = heap_alloc(2);
  u32  got_at = (u32)base;
  // TAKE valores originais
  Term x = heap_take(s_loc + 0);
  Term y = heap_take(s_loc + 1);
  // Armazena nos GOTs
  heap_write(got_at + 0, x);
  heap_write(got_at + 1, y);
  // Reescreve campos do SUP ORIGINAL
  heap_write(s_loc + 0, term_new_got(got_at + 0));
  heap_write(s_loc + 1, term_new_got(got_at + 1));
  // REUTILIZA o SUP original (s_loc, não at+2!)
  Term res = term_new(0, SUP, s_lab, s_loc);
  heap_subst_var(loc, res);
  return res;
}