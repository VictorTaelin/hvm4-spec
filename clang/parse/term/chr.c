fn Term parse_term_chr(PState *s, u32 depth) {
  if (!parse_match(s, "\'")) return 0;
  // parse_advance(s);
  u32 c;
  if (parse_peek(s) == '\\') {
    parse_advance(s);
    c = (u32)(u8)parse_peek(s);
    parse_advance(s);
  } else {
    c = parse_utf8(s);
  }
  parse_consume(s, "'");
  Term n = term_new_num(c);
  return term_new_ctr(NAM_CHR, 1, &n);
}
