fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_par(PState *s, u32 depth) {
  if (!parse_match(s, "(")) return 0;
  Term term = parse_term(s, depth);
  parse_consume(s, ")");
  return term;
}
