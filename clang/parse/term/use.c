fn Term parse_term_use(PState *s, u32 depth) {
  if (!parse_match(s, "λ")) return 0;
  parse_skip(s);
  if (!parse_match(s, "{")) return 0;
  Term f = parse_term(s, depth);
  parse_skip(s);
  if (!parse_match(s, "}")) return 0;
  return term_new_use(f);
}
