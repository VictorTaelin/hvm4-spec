fn Term parse_term(PState *s, u32 depth);

fn Term parse_term_lam(PState *s, u32 depth) {
  parse_skip(s);
  if (parse_peek(s) == '{') {
    parse_consume(s, "{");
    parse_skip(s);
    if (parse_peek(s) == '}') {
      parse_consume(s, "}");
      return term_new_era();
    }
    // Check for SWI: λ{123: f; g} (but not nat pattern like 0n:)
    if (isdigit(parse_peek(s))) {
      u32 sav = s->pos;
      u32 num = 0;
      while (isdigit(parse_peek(s))) {
        num = num * 10 + (parse_peek(s) - '0');
        parse_advance(s);
      }
      parse_skip(s);
      if (parse_peek(s) != 'n') {
        parse_consume(s, ":");
        Term f = parse_term(s, depth);
        parse_skip(s);
        parse_consume(s, ";");
        Term g = parse_term(s, depth);
        parse_skip(s);
        parse_consume(s, "}");
        return term_new_swi(num, f, g);
      }
      s->pos = sav;
    }
    // Check for MAT: λ{#Name: ...} or sugar patterns, or USE: λ{f}
    Term mat = term_new_era();
    Term *tip = &mat;
    while (1) {
      parse_skip(s);
      u32 nam = 0;
      if (parse_peek(s) == '#') {
        parse_advance(s);
        nam = parse_name(s);
      } else if (parse_peek(s) == '0' && parse_peek_at(s, 1) == 'n') {
        parse_advance(s); parse_advance(s); nam = NAM_ZER;
      } else if (isdigit(parse_peek(s))) {
        while (isdigit(parse_peek(s))) parse_advance(s);
        if (parse_peek(s) == 'n' && parse_peek_at(s, 1) == '+') {
          parse_advance(s); parse_advance(s); nam = NAM_SUC;
        }
      } else if (parse_peek(s) == '[' && parse_peek_at(s, 1) == ']') {
        parse_advance(s); parse_advance(s); nam = NAM_NIL;
      } else if (parse_peek(s) == '<' && parse_peek_at(s, 1) == '>') {
        parse_advance(s); parse_advance(s); nam = NAM_CON;
      }
      if (nam) {
        parse_skip(s);
        parse_consume(s, ":");
        Term val = parse_term(s, depth);
        parse_skip(s);
        parse_match(s, ";");
        u64 loc = heap_alloc(2);
        HEAP[loc + 0] = val;
        HEAP[loc + 1] = term_new_era();
        *tip = term_new(0, MAT, nam, loc);
        tip = &HEAP[loc + 1];
      } else if (parse_peek(s) == '}') {
        parse_consume(s, "}");
        return mat;
      } else {
        // USE: λ{f} - single term without # prefix
        if (mat == term_new_era()) {
          // No MAT cases yet, this is a USE
          Term f = parse_term(s, depth);
          parse_skip(s);
          parse_consume(s, "}");
          return term_new_use(f);
        }
        // Default case for MAT
        *tip = parse_term(s, depth);
        parse_consume(s, "}");
        return mat;
      }
    }
  }
  u32 nam = parse_name(s);
  parse_consume(s, ".");
  parse_bind_push(nam, depth, 0);
  u64  loc  = heap_alloc(1);
  Term body = parse_term(s, depth + 1);
  HEAP[loc] = body;
  parse_bind_pop();
  return term_new(0, LAM, depth, loc);
}
