fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_app(Term f, PState *s, u32 depth) {
  parse_skip(s);
  if (parse_peek(s) != '(') {
    return f;
  }
  parse_consume(s, "(");
  if (parse_peek(s) == ')') {
    parse_consume(s, ")");
    return parse_term_app(f, s, depth);
  }
  while (1) {
    Term arg = parse_term(s, depth);
    f = term_new_app(f, arg);
    parse_skip(s);
    if (parse_peek(s) == ',') {
      parse_consume(s, ",");
      continue;
    }
    if (parse_peek(s) == ')') {
      parse_consume(s, ")");
      break;
    }
    parse_error(s, "',' or ')'", parse_peek(s));
  }
  return parse_term_app(f, s, depth);
}
