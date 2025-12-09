fn Term parse_term_ref(PState *s, u32 depth) {
  if (!parse_match(s, "@")) return 0;
  return term_new_ref(parse_name_ref(s));
}
