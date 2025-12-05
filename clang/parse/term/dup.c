fn Term parse_term(PState *s, u32 depth);
fn Term parse_term_uns(PState *s, u32 depth);

// Helper: parse dup after & is consumed. Expects: [(label)] [=val;] body
// If val is provided (non-zero loc), uses it; otherwise parses "= val;" from input.
// For λx&L.F sugar, val_loc points to where VAR(0) should go.
fn Term parse_dup_body(PState *s, u32 nam, u32 cloned, u32 depth, u64 val_loc) {
  parse_skip(s);
  // Dynamic label: (expr)
  if (parse_peek(s) == '(') {
    parse_consume(s, "(");
    Term lab_term = parse_term(s, depth);
    parse_consume(s, ")");
    Term val;
    if (val_loc) {
      val = HEAP[val_loc];
      parse_consume(s, ".");
    } else {
      parse_consume(s, "=");
      val = parse_term(s, depth);
      parse_skip(s);
      parse_match(s, ";");
    }
    parse_skip(s);
    parse_bind_push(nam, depth, 0xFFFFFF, cloned);
    Term body = parse_term(s, depth + 2);
    u32 uses0 = parse_bind_get_uses0();
    u32 uses1 = parse_bind_get_uses1();
    parse_bind_pop();
    if (cloned && uses0 > 1) {
      body = parse_auto_dup(body, 1, uses0, VAR, 0);
    }
    if (cloned && uses1 > 1) {
      body = parse_auto_dup(body, 0, uses1, VAR, 0);
    }
    u64 loc0   = heap_alloc(1);
    u64 loc1   = heap_alloc(1);
    HEAP[loc1] = body;
    Term lam1  = term_new(0, LAM, depth + 1, loc1);
    HEAP[loc0] = lam1;
    Term lam0  = term_new(0, LAM, depth, loc0);
    return term_new_ddu(lab_term, val, lam0);
  }
  // Static label (or auto if next is = or .)
  u32 lab;
  char c = parse_peek(s);
  if (c == '=' || c == '.') {
    lab = PARSE_FRESH_LAB++;
  } else {
    lab = parse_name(s);
  }
  Term val;
  u64 loc = heap_alloc(2);
  if (val_loc) {
    HEAP[loc] = HEAP[val_loc];
    parse_consume(s, ".");
  } else {
    parse_consume(s, "=");
    HEAP[loc] = parse_term(s, depth);
    parse_skip(s);
    parse_match(s, ";");
  }
  parse_skip(s);
  parse_bind_push(nam, depth, lab, cloned);
  Term body     = parse_term(s, depth + 1);
  u32 uses      = parse_bind_get_uses();
  u32 uses0     = parse_bind_get_uses0();
  u32 uses1     = parse_bind_get_uses1();
  if (!cloned && uses > 2) {
    fprintf(stderr, "\033[1;31mPARSE_ERROR\033[0m\n");
    fprintf(stderr, "- dup variable '"); print_name(stderr, nam);
    fprintf(stderr, "' used %d times (max 2 with ₀ and ₁)\n", uses);
    exit(1);
  }
  if (cloned && uses1 > 1) {
    body = parse_auto_dup(body, 0, uses1, CO1, lab);
  }
  if (cloned && uses0 > 1) {
    body = parse_auto_dup(body, 0, uses0, CO0, lab);
  }
  HEAP[loc + 1] = body;
  parse_bind_pop();
  return term_new(0, DUP, lab, loc);
}

fn Term parse_term_dup(PState *s, u32 depth) {
  parse_skip(s);
  // Check for unscoped binding: ! ${f, v}; body
  if (parse_match(s, "$")) {
    return parse_term_uns(s, depth);
  }
  // Check for !!x = val or !!&x = val (strict let, optionally cloned)
  int strict = parse_match(s, "!");
  parse_skip(s);
  // Check for cloned: & comes BEFORE name
  // Cloned let: ! &x = val
  // Cloned dup: ! &X &L = val  or  ! &X &(L) = val
  // Regular dup: !x& = val
  u32 cloned = 0;
  if (parse_peek(s) == '&') {
    parse_advance(s);  // consume &
    parse_skip(s);
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
      fprintf(stderr, "- variable '"); print_name(stderr, nam);
      fprintf(stderr, "' used %d times (not cloned)\n", uses);
      fprintf(stderr, "- hint: use ! & to allow multiple uses\n");
      exit(1);
    }
    // Apply auto-dup transformation for cloned variables with multiple uses
    if (cloned && uses > 1) {
      body = parse_auto_dup(body, 0, uses, VAR, 0);
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
  // Cloned DUP: !&X &label = val; body  or  !&X & = val; body (auto-label)
  // Dynamic DUP: !x&(lab) = val; body  (lab is an expression)
  // Cloned Dynamic DUP: !&X &(lab) = val; body
  parse_consume(s, "&");
  return parse_dup_body(s, nam, cloned, depth, 0);
}
