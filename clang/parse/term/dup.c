fn Term parse_term(PState *s, u32 depth);

// [L|(L)] = v; b
fn Term parse_dup_label_body(PState *s, u32 nam, u32 cloned, u32 depth) {
  parse_skip(s);
  if (parse_peek(s) == '(') {
    parse_consume(s, "(");
    Term lab_term = parse_term(s, depth);
    parse_consume(s, ")");
    parse_consume(s, "=");
    Term val = parse_term(s, depth);
    parse_skip(s);
    parse_match(s, ";");
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
  u32 lab;
  if (parse_peek(s) == '=') {
    lab = PARSE_FRESH_LAB++;
  } else {
    lab = parse_name(s);
  }
  parse_skip(s);
  parse_consume(s, "=");
  u64 loc = heap_alloc(2);
  HEAP[loc] = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ";");
  parse_skip(s);
  parse_bind_push(nam, depth, lab, cloned);
  Term body     = parse_term(s, depth + 1);
  u32 uses      = parse_bind_get_uses();
  u32 uses0     = parse_bind_get_uses0();
  u32 uses1     = parse_bind_get_uses1();
  if (!cloned && uses > 2) {
    parse_error(s, PERR_AFFINE_DUP(nam, uses));
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

// [&]x &[L|(L)] = v; b
fn Term parse_term_dup_stmt_body(PState *s, u32 depth) {
  parse_skip(s);
  u32 cloned = parse_match(s, "&");
  u32 nam = parse_name(s);
  parse_skip(s);
  if (!parse_match(s, "&")) return 0;
  return parse_dup_label_body(s, nam, cloned, depth);
}
