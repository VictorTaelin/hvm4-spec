fn Term parse_term_var(PState *s, u32 depth) {
  parse_skip(s);
  u32 nam = parse_name(s);
  int idx;
  u32 lab;
  u32 cloned;
  parse_bind_lookup(nam, depth, &idx, &lab, &cloned);
  parse_skip(s);
  int side = parse_match(s, "₀") ? 0 : parse_match(s, "₁") ? 1 : -1;
  parse_skip(s);
  // Error if variable is not found in bindings
  if (idx < 0) {
    char name_buf[16];
    nick_to_str(nam, name_buf, sizeof(name_buf));
    fprintf(stderr, "\033[1;31mPARSE_ERROR\033[0m\n");
    fprintf(stderr, "- undefined variable '%s'\n", name_buf);
    exit(1);
  }
  u32 val = (u32)idx;
  u8  tag = (side == 0) ? CO0 : (side == 1) ? CO1 : VAR;
  return term_new(0, tag, lab, val);
}
