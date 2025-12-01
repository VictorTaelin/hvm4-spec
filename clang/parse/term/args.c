fn Term parse_term(PState *s, u32 depth);

// Parses argument list: (a,b,c,...) and returns count
// Stores args in provided array
fn u32 parse_term_args(PState *s, u32 depth, Term *args, u32 max_args) {
  u32 cnt = 0;
  parse_skip(s);
  if (parse_peek(s) != ')') {
    while (1) {
      if (cnt >= max_args) {
        parse_error(s, "too many arguments", parse_peek(s));
      }
      args[cnt++] = parse_term(s, depth);
      parse_skip(s);
      if (parse_peek(s) == ',') {
        parse_consume(s, ",");
        continue;
      }
      break;
    }
  }
  parse_consume(s, ")");
  return cnt;
}
