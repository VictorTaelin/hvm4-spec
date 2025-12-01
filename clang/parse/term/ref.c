fn Term parse_term_ref(PState *s) {
  return term_new_ref(parse_name(s));
}
