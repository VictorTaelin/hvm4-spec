// fn Term parse_term(Term f, PState *s, u32 depth, int min_prec);

fn Term parse_term_par(Term f, PState *s, u32 depth, int min_prec) {
  (void)f; (void)min_prec;
  if (!parse_match(s, "(")) return 0;
  Term term = parse_term(NONE, s, depth, 0);
  parse_consume(s, ")");
  return term;
}
