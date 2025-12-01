fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_dup(PState *s, u32 depth) {
  parse_skip(s);
  u32 nam = parse_name(s);
  parse_consume(s, "&");
  u32  lab = parse_name(s);
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
