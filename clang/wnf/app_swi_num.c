// (λ{n: z; s} #n)
// --------------- app-swi-match
// z
//
// (λ{n: z; s} #m)
// --------------- app-swi-miss
// (s #m)
fn Term wnf_app_swi_num(Term swi, Term num) {
  ITRS++;
  u32  loc = term_val(swi);
  Term z   = HEAP[loc + 0];
  Term s   = HEAP[loc + 1];
  if (term_ext(swi) == term_val(num)) {
    return z;
  } else {
    return term_new_app(s, num);
  }
}
