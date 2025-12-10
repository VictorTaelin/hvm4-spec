fn Term parse_term(PState *s, u32 depth);
fn Term parse_term_lam_go(PState *s, u32 depth);

// [&]x[,..].b
fn Term parse_term_lam_simple(PState *s, u32 depth) {
  parse_skip(s);
  u32 cloned = parse_match(s, "&");
  u32 nam    = parse_name(s);
  parse_skip(s);
  parse_bind_push(nam, depth, 0, cloned);
  Term body;
  if (parse_match(s, ",")) {
    body = parse_term_lam_go(s, depth + 1);
  } else {
    parse_consume(s, ".");
    body = parse_term(s, depth + 1);
  }
  u32 uses = parse_bind_get_uses();
  if (!cloned && uses > 1) {
    parse_error(s, PERR_AFFINE(nam, uses, "λ&"));
  }
  if (cloned && uses > 1) {
    body = parse_auto_dup(body, 0, uses, VAR, 0);
  }
  parse_bind_pop();
  u64 loc = heap_alloc(1);
  HEAP[loc] = body;
  return term_new(0, LAM, depth, loc);
}

// [&]x&[L][,..].b
fn Term parse_term_lam_dup_static(PState *s, u32 depth) {
  parse_skip(s);
  u32 cloned = parse_match(s, "&");
  u32 nam    = parse_name(s);
  parse_skip(s);
  if (!parse_match(s, "&")) return 0;
  parse_skip(s);
  if (parse_peek(s) == '(') return 0;
  u32 lab;
  char c = parse_peek(s);
  if (c == ',' || c == '.') {
    lab = PARSE_FRESH_LAB++;
  } else {
    lab = parse_name(s);
  }
  parse_skip(s);
  parse_bind_push(nam, depth + 1, lab, cloned);
  Term body;
  if (parse_match(s, ",")) {
    body = parse_term_lam_go(s, depth + 2);
  } else {
    parse_consume(s, ".");
    body = parse_term(s, depth + 2);
  }
  u32 uses  = parse_bind_get_uses();
  u32 uses0 = parse_bind_get_uses0();
  u32 uses1 = parse_bind_get_uses1();
  parse_bind_pop();
  if (!cloned && uses > 2) {
    parse_error(s, PERR_AFFINE_DUP(nam, uses));
  }
  if (cloned && uses1 > 1) {
    body = parse_auto_dup(body, 0, uses1, CO1, lab);
  }
  if (cloned && uses0 > 1) {
    body = parse_auto_dup(body, 0, uses0, CO0, lab);
  }
  u64 dup_loc = heap_alloc(2);
  HEAP[dup_loc + 0] = term_new(0, VAR, 0, 0);
  HEAP[dup_loc + 1] = body;
  u64 lam_loc = heap_alloc(1);
  HEAP[lam_loc] = term_new(0, DUP, lab, dup_loc);
  return term_new(0, LAM, depth, lam_loc);
}

// [&]x&(L)[,..].b
fn Term parse_term_lam_dup_dyn(PState *s, u32 depth) {
  parse_skip(s);
  u32 cloned = parse_match(s, "&");
  u32 nam    = parse_name(s);
  parse_skip(s);
  if (!parse_match(s, "&")) return 0;
  parse_skip(s);
  if (!parse_match(s, "(")) return 0;
  Term lab_term = parse_term(s, depth + 1);
  parse_consume(s, ")");
  parse_skip(s);
  parse_bind_push(nam, depth + 1, 0xFFFFFF, cloned);
  Term body;
  if (parse_match(s, ",")) {
    body = parse_term_lam_go(s, depth + 3);
  } else {
    parse_consume(s, ".");
    body = parse_term(s, depth + 3);
  }
  u32 uses0 = parse_bind_get_uses0();
  u32 uses1 = parse_bind_get_uses1();
  parse_bind_pop();
  if (cloned && uses0 > 1) {
    body = parse_auto_dup(body, 1, uses0, VAR, 0);
  }
  if (cloned && uses1 > 1) {
    body = parse_auto_dup(body, 0, uses1, VAR, 0);
  }
  u64 loc1 = heap_alloc(1);
  HEAP[loc1] = body;
  u64 loc0 = heap_alloc(1);
  HEAP[loc0] = term_new(0, LAM, depth + 2, loc1);
  Term ddu = term_new_ddu(lab_term, term_new(0, VAR, 0, 0), term_new(0, LAM, depth + 1, loc0));
  u64 lam_loc = heap_alloc(1);
  HEAP[lam_loc] = ddu;
  return term_new(0, LAM, depth, lam_loc);
}

fn Term parse_term_lam_go(PState *s, u32 depth) {
  TermParser alts[] = {
    parse_term_lam_dup_dyn,
    parse_term_lam_dup_static,
    parse_term_lam_simple,
    NULL
  };
  return parse_choice(s, depth, alts);
}

fn Term parse_term_lam_body(PState *s, u32 depth) {
  return parse_term_lam_go(s, depth);
}
