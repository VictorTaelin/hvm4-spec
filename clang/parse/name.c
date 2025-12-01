fn u32 parse_name(PState *s) {
  parse_skip(s);
  char c = parse_peek(s);
  if (!nick_is_init(c)) {
    parse_error(s, "name", c);
  }
  u32 k = 0;
  while (nick_is_char(parse_peek(s))) {
    c = parse_peek(s);
    k = ((k << 6) + nick_letter_to_b64(c)) & EXT_MASK;
    parse_advance(s);
  }
  parse_skip(s);
  return k;
}
