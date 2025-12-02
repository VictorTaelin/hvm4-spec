fn Term parse_term_chr(PState *s) {
  parse_advance(s);
  char c = parse_peek(s);
  if (c == '\\') { parse_advance(s); c = parse_peek(s); }
  parse_advance(s);
  parse_consume(s, "'");
  Term n = term_new_num((u32)c);
  return term_new_ctr(NAM_CHR, 1, &n);
}
