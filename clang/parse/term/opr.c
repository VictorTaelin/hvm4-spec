fn Term parse_term(PState *s, u32 depth);

// Try to match an infix operator. Returns the op code or -1 if no match.
// If matched, advances the parser past the operator.
fn int parse_term_opr_match(PState *s) {
  parse_skip(s);
  char c = parse_peek(s);
  char c1 = parse_peek_at(s, 1);

  // Two-character operators
  if (c == '=' && c1 == '=') { parse_advance(s); parse_advance(s); return OP_EQ; }
  if (c == '!' && c1 == '=') { parse_advance(s); parse_advance(s); return OP_NE; }
  if (c == '<' && c1 == '=') { parse_advance(s); parse_advance(s); return OP_LE; }
  if (c == '>' && c1 == '=') { parse_advance(s); parse_advance(s); return OP_GE; }
  if (c == '<' && c1 == '<') { parse_advance(s); parse_advance(s); return OP_LSH; }
  if (c == '>' && c1 == '>') { parse_advance(s); parse_advance(s); return OP_RSH; }
  if (c == '&' && c1 == '&') { parse_advance(s); parse_advance(s); return OP_AND; }
  if (c == '|' && c1 == '|') { parse_advance(s); parse_advance(s); return OP_OR; }

  // Single-character operators (check they're not part of something else)
  if (c == '+') { parse_advance(s); return OP_ADD; }
  if (c == '-') { parse_advance(s); return OP_SUB; }
  if (c == '*') { parse_advance(s); return OP_MUL; }
  if (c == '/') { parse_advance(s); return OP_DIV; }
  if (c == '%') { parse_advance(s); return OP_MOD; }
  if (c == '^') { parse_advance(s); return OP_XOR; }
  if (c == '~') { parse_advance(s); return OP_NOT; }
  if (c == '<') { parse_advance(s); return OP_LT; }
  if (c == '>') { parse_advance(s); return OP_GT; }

  return -1;
}
