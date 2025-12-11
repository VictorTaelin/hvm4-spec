fn Term parse_term(Term f, PState *s, u32 depth, int min_prec);

fn Term parse_postfix_cons(Term f, PState *s, u32 depth, int min_prec) {
  // Make <> the loosest precedence so RHS keeps normal infix grouping,
  // e.g. `1 <> 2+3 <> []` → 1 <> (2+3) <> [].
  int prec = 0;
  if (prec < min_prec) return 0;
  if (!parse_match(s, "<>")) return 0;
  // Right-associative: parse RHS with prec (not prec+1) so another <>
  // at the same precedence can attach on the right.
  Term t = parse_term(NONE, s, depth, prec);
  Term a[2] = {f, t};
  return term_new_ctr(NAM_CON, 2, a);
}
