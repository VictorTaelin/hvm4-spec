fn Term parse_term(PState *s, u32 depth);

// [&]x = v; b
fn Term parse_term_let_core(PState *s, u32 depth, int strict) {
  parse_skip(s);
  u32 cloned = parse_match(s, "&");
  u32 nam = parse_name(s);
  parse_skip(s);
  if (!parse_match(s, "=")) return 0;
  Term val = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ";");
  parse_bind_push(nam, depth, 0, cloned);
  u64  loc  = heap_alloc(1);
  Term body = parse_term(s, depth + 1);
  u32  uses = parse_bind_get_uses();
  if (!cloned && uses > 1) {
    parse_error(s, PERR_AFFINE(nam, uses, "! &"));
  }
  if (cloned && uses > 1) {
    body = parse_auto_dup(body, 0, uses, VAR, 0);
  }
  HEAP[loc] = body;
  parse_bind_pop();
  Term lam = term_new(0, LAM, depth, loc);
  if (strict) {
    lam = term_new_use(lam);
  }
  return term_new_app(lam, val);
}

fn Term parse_term_let_body(PState *s, u32 depth) {
  return parse_term_let_core(s, depth, 0);
}

// ![&]x = v; b
fn Term parse_term_strict_let_body(PState *s, u32 depth) {
  if (!parse_match(s, "!")) return 0;
  return parse_term_let_core(s, depth, 1);
}
