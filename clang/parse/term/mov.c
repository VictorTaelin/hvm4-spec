fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_mov(PState *s, u32 depth) {
  parse_skip(s);
  u32 nam = parse_name(s);
  parse_skip(s);
  parse_consume(s, "=");
  Term val = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ";");
  parse_skip(s);
  parse_bind_push(nam, depth, 0, PBIND_LAM, 1);
  u64  loc  = heap_alloc(1);
  Term body = parse_term(s, depth + 1);
  body = parse_auto_dup(body, depth + 1, depth + 1, BJV, 0);
  HEAP[loc] = body;
  parse_bind_pop();
  Term lam = term_new(0, LAM, depth + 1, loc);
  return term_new_app(lam, val);
}
