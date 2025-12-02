fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_sup(PState *s, u32 depth) {
  parse_skip(s);
  if (parse_peek(s) == '{') {
    parse_consume(s, "{");
    parse_consume(s, "}");
    return term_new_era();
  }
  if (parse_peek(s) == '(') {
    parse_consume(s, "(");
    Term lab = parse_term(s, depth);
    parse_consume(s, ")");
    parse_consume(s, "{");
    Term tm0 = parse_term(s, depth);
    parse_skip(s);
    parse_match(s, ",");
    parse_skip(s);
    Term tm1 = parse_term(s, depth);
    parse_consume(s, "}");
    return term_new_app(term_new_app(term_new_app(term_new_ref(table_find("sup", 3)), lab), tm0), tm1);
  }
  u32 lab = parse_name(s);
  parse_consume(s, "{");
  Term tm0 = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ",");
  parse_skip(s);
  Term tm1 = parse_term(s, depth);
  parse_consume(s, "}");
  return term_new_sup(lab, tm0, tm1);
}
