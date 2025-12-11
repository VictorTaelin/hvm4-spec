fn Term parse_term_atom(PState *s, u32 depth);
fn Term parse_term(Term f, PState *s, u32 depth, int min_prec);

fn Term parse_postfix_and(Term f, PState *s, u32 depth, int min_prec) {
  int prec = 2; // same precedence as &&
  if (prec < min_prec) return 0;
  if (!parse_match(s, ".&.")) return 0;
  // Left-associative: RHS with prec+1 so another .&. can't attach on RHS.
  Term rhs = parse_term(NONE, s, depth, prec + 1);
  return term_new_and(f, rhs);
}
