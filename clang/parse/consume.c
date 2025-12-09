fn void parse_consume(PState *s, const char *str) {
  parse_skip(s);
  if (!parse_match(s, str)) {
    parse_error(s, PERR_EXPECTED(str, parse_peek(s)));
  }
  parse_skip(s);
}
