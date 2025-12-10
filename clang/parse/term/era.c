// {}
fn Term parse_term_era_amp(PState *s, u32 depth) {
  parse_skip(s);
  if (!parse_match(s, "{")) return 0;
  parse_skip(s);
  if (!parse_match(s, "}")) return 0;
  return term_new_era();
}

// {}
fn Term parse_term_era_lam(PState *s, u32 depth) {
  parse_skip(s);
  if (!parse_match(s, "{")) return 0;
  parse_skip(s);
  if (!parse_match(s, "}")) return 0;
  return term_new_era();
}
