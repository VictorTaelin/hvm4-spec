fn u32 parse_name(PState *s) {
  parse_skip(s);
  char c = parse_peek(s);
  if (!nick_is_init(c)) {
    parse_error(s, PERR_EXPECTED("name", c));
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

// Like parse_name, but returns a unique ID from the global table.
// Used for function definitions and references.
fn u32 parse_name_ref(PState *s) {
  parse_skip(s);
  char c = parse_peek(s);
  if (!nick_is_init(c)) {
    parse_error(s, PERR_EXPECTED("name", c));
  }
  u32 start = s->pos;
  while (nick_is_char(parse_peek(s))) {
    parse_advance(s);
  }
  u32 len = s->pos - start;
  u32 id  = table_find(s->src + start, len);
  parse_skip(s);
  return id;
}
