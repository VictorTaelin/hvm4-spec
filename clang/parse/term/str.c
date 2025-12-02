fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_str(PState *s, u32 depth) {
  parse_advance(s);
  Term t = term_new_ctr(NAM_NIL, 0, 0);
  char cs[4096]; u32 n = 0;
  while (parse_peek(s) != '"') {
    char c = parse_peek(s);
    if (c == '\\') { parse_advance(s); c = parse_peek(s); }
    cs[n++] = c;
    parse_advance(s);
  }
  parse_advance(s);
  for (int i = n - 1; i >= 0; i--) {
    Term c = term_new_num((u32)(unsigned char)cs[i]);
    Term h = term_new_ctr(NAM_CHR, 1, &c);
    Term a[2] = {h, t};
    t = term_new_ctr(NAM_CON, 2, a);
  }
  return t;
}
