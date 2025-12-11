fn Term parse_term(Term f, PState *s, u32 depth, int min_prec);

fn Term parse_term_ctr(Term f, PState *s, u32 depth, int min_prec) {
  (void)f; (void)min_prec;
  if (!parse_match(s, "#")) return 0;
  u32  nam = parse_name(s);
  Term args[16];
  u32  cnt = 0;
  parse_skip(s);
  if (parse_match(s, "{")) {
    parse_skip(s);
    while (parse_peek(s) != '}') {
      args[cnt++] = parse_term(NONE, s, depth, 0);
      parse_skip(s);
      parse_match(s, ",");  // optional comma
      parse_skip(s);
    }
    parse_consume(s, "}");
  }
  return term_new_ctr(nam, cnt, args);
}
