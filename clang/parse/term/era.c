fn Term parse_term_era(PState *s, u32 depth) {
  parse_skip(s);
  if (parse_match(s, "λ") || parse_match(s, "&")) {
    parse_skip(s);
    if (!parse_match(s, "{")) return 0;
    parse_skip(s);
    if (!parse_match(s, "}")) return 0;

    return term_new_era();
  }
  return 0;
}
