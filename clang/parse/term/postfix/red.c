fn Term parse_term(Term f, PState *s, u32 depth, int min_prec);

fn Term parse_postfix_red(Term f, PState *s, u32 depth, int min_prec) {
  // Treat ~> as right-associative and low precedence (like <>).
  int prec = 0;
  if (prec < min_prec) return 0;
  if (!parse_match(s, "~>")) return 0;
  // Right-associative: RHS with prec so another ~> at the same precedence
  // can chain on the right.
  Term g = parse_term(NONE, s, depth, prec);
  return term_new_red(f, g);
}
