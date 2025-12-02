fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_dup(PState *s, u32 depth) {
  parse_skip(s);
  // Check for &(lab)=val form (dup primitive call)
  // Must be &( not just &
  if (parse_peek(s) == '&' && parse_peek_at(s, 1) == '(') {
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
  // Check for !!x = val or !!&x = val (strict let, optionally cloned)
  int strict = parse_match(s, "!");
  parse_skip(s);
  // Check for cloned let: ! &x = val or !!&x = val
  // This must be distinguished from regular dup: !x& = val
  // Cloned: & comes BEFORE name (! &x = val)
  // Regular dup: & comes AFTER name (!x& = val)
  u32 cloned = 0;
  if (parse_peek(s) == '&') {
    parse_advance(s);  // consume &
    parse_skip(s);
    // After &, we expect a name for cloned let
    cloned = 1;
  }
  u32 nam = parse_name(s);
  parse_skip(s);
  // Check for let sugar: ! x = val; body  →  (λx.body)(val)
  // Or cloned let: ! &x = val; body
  if (parse_peek(s) == '=') {
    parse_advance(s);
    Term val = parse_term(s, depth);
    parse_skip(s);
    parse_match(s, ";");
    parse_bind_push(nam, depth, 0, cloned);
    u64  loc  = heap_alloc(1);
    Term body = parse_term(s, depth + 1);
    u32  uses = parse_bind_get_uses();
    // Check for affinity violation on non-cloned variables
    if (!cloned && uses > 1) {
      fprintf(stderr, "\033[1;31mPARSE_ERROR\033[0m\n");
      fprintf(stderr, "- variable used %d times (not cloned)\n", uses);
      fprintf(stderr, "- hint: use ! & to allow multiple uses\n");
      exit(1);
    }
    // Apply auto-dup transformation for cloned variables with multiple uses
    if (cloned && uses > 1) {
      body = parse_auto_dup(body, 0, uses);
    }
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
  parse_bind_push(nam, depth, lab, 0);  // DUP bindings are not cloned (they're already duplicated)
  u64 loc       = heap_alloc(2);
  HEAP[loc + 0] = val;
  Term body     = parse_term(s, depth + 1);
  u32 uses = parse_bind_get_uses();
  // Check for affinity violation on dup bindings (they can only be used via CO0/CO1)
  if (uses > 2) {
    fprintf(stderr, "\033[1;31mPARSE_ERROR\033[0m\n");
    fprintf(stderr, "- dup variable used %d times (max 2 with ₀ and ₁)\n", uses);
    exit(1);
  }
  HEAP[loc + 1] = body;
  parse_bind_pop();
  return term_new(0, DUP, lab, loc);
}
