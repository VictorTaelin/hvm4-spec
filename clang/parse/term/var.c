fn Term parse_term_var(PState *s, u32 depth) {
  parse_skip(s);
  u32 nam = parse_name(s);
  int idx;
  u32 lab;
  parse_bind_lookup(nam, depth, &idx, &lab);
  parse_skip(s);
  int side = parse_match(s, "₀") ? 0 : parse_match(s, "₁") ? 1 : -1;
  parse_skip(s);
  u32 val = (idx >= 0) ? (u32)idx : nam;
  u8  tag = (side == 0) ? CO0 : (side == 1) ? CO1 : VAR;
  return term_new(0, tag, lab, val);
}
