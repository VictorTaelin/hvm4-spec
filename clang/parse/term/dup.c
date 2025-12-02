fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_dup(PState *s, u32 depth) {
  parse_skip(s);
  if (parse_peek(s) == '&') {
    parse_consume(s, "&");
    parse_consume(s, "(");
    Term lab = parse_term(s, depth);
    parse_consume(s, ")");
    parse_consume(s, "=");
    Term val = parse_term(s, depth);
    parse_skip(s);
    parse_match(s, ";");
    parse_skip(s);
    Term body = parse_term(s, depth);
    return term_new_app(term_new_app(term_new_app(term_new_ref(table_find("dup", 3)), lab), val), body);
  }
  // Check for !!x = val; body  →  (λ{λx.body(x)})(val)
  int strict = parse_match(s, "!");
  u32 nam = parse_name(s);
  parse_skip(s);
  // Check for let sugar: ! x = val; body  →  (λx.body)(val)
  if (parse_peek(s) == '=') {
    parse_advance(s);
    Term val = parse_term(s, depth);
    parse_skip(s);
    parse_match(s, ";");
    parse_bind_push(nam, depth, 0);
    u64  loc  = heap_alloc(1);
    Term body = parse_term(s, depth + 1);
    HEAP[loc] = body;
    parse_bind_pop();
    Term lam = term_new(0, LAM, depth, loc);
    if (strict) {
      // !!x = val; body  →  (λ{λx.body(x)})(val)
      lam = term_new_use(lam);
    }
    return term_new_app(lam, val);
  }
  // Regular DUP: !x&label = val; body  or  !x& = val; body (auto-label)
  parse_consume(s, "&");
  parse_skip(s);
  u32 lab;
  if (parse_peek(s) == '=') {
    lab = PARSE_FRESH_LAB++;
  } else {
    lab = parse_name(s);
  }
  parse_consume(s, "=");
  Term val = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ";");
  parse_skip(s);
  parse_bind_push(nam, depth, lab);
  u64 loc       = heap_alloc(2);
  HEAP[loc + 0] = val;
  Term body     = parse_term(s, depth + 1);
  HEAP[loc + 1] = body;
  parse_bind_pop();
  return term_new(0, DUP, lab, loc);
}
