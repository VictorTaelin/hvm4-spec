fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_pri(PState *s, u32 depth) {
  u32  nam = parse_name(s);
  parse_consume(s, "(");
  Term args[16];
  u32  cnt = parse_term_args(s, depth, args, 16);
  return term_new_pri(nam, cnt, args);
}
