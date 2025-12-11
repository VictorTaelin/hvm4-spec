// fn Term parse_term(Term f, PState *s, u32 depth, int min_prec);

// ^name or ^(f x)
fn Term parse_term_nam(Term f, PState *s, u32 depth, int min_prec) {
  (void)f; (void)min_prec;
  if (!parse_match(s, "^")) return 0;
  parse_skip(s);
  if (parse_peek(s) == '(') {
    // ^(f x) -> DRY(f, x)
    parse_consume(s, "(");
    Term f = parse_term(NONE, s, depth, 0);
    Term x = parse_term(NONE, s, depth, 0);
    parse_consume(s, ")");
    return term_new_dry(f, x);
  } else {
    // ^name -> NAM(name)
    u32 nam = parse_name(s);
    return term_new(0, NAM, nam, 0);
  }
}
