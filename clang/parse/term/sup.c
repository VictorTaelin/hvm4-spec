fn Term parse_term(PState *s, u32 depth);

// [L|(L)]{a,b}
fn Term parse_term_sup_body(PState *s, u32 depth) {
  int  dyn      = parse_peek(s) == '(';
  Term lab_term = 0;
  u32  lab      = 0;
  if (dyn) {
    parse_consume(s, "(");
    lab_term = parse_term(s, depth);
    parse_consume(s, ")");
  } else {
    lab = parse_name(s);
  }
  parse_skip(s);
  if (!parse_match(s, "{")) return 0;
  Term tm0 = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ",");
  Term tm1 = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ",");
  parse_consume(s, "}");
  return dyn ? term_new_dsu(lab_term, tm0, tm1) : term_new_sup(lab, tm0, tm1);
}
