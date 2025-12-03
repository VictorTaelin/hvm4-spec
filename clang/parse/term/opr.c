fn Term parse_term(PState *s, u32 depth);

// Returns operator precedence (higher = binds tighter)
fn int parse_term_opr_prec(int op) {
  switch (op) {
    case OP_OR:  return 1;
    case OP_AND: return 2;
    case OP_EQ: case OP_NE: return 3;
    case OP_LT: case OP_LE: case OP_GT: case OP_GE: return 4;
    case OP_LSH: case OP_RSH: return 5;
    case OP_ADD: case OP_SUB: return 6;
    case OP_MUL: case OP_DIV: case OP_MOD: return 7;
    case OP_XOR: return 8;
    default: return 0;
  }
}

// Peek at next operator without consuming. Returns op code or -1.
fn int parse_term_opr_peek(PState *s) {
  parse_skip(s);
  char c = parse_peek(s);
  char c1 = parse_peek_at(s, 1);

  if (c == '=' && c1 == '=') return OP_EQ;
  if (c == '!' && c1 == '=') return OP_NE;
  if (c == '<' && c1 == '=') return OP_LE;
  if (c == '>' && c1 == '=') return OP_GE;
  if (c == '<' && c1 == '<') return OP_LSH;
  if (c == '>' && c1 == '>') return OP_RSH;
  if (c == '&' && c1 == '&') return OP_AND;
  if (c == '|' && c1 == '|') return OP_OR;

  if (c == '+') return OP_ADD;
  if (c == '-') return OP_SUB;
  if (c == '*') return OP_MUL;
  if (c == '/') return OP_DIV;
  if (c == '%') return OP_MOD;
  if (c == '^') return OP_XOR;
  if (c == '~') return OP_NOT;
  if (c == '<') return OP_LT;
  if (c == '>') return OP_GT;

  return -1;
}

// Consume an operator (call after peek confirms one exists)
fn void parse_term_opr_consume(PState *s, int op) {
  parse_skip(s);
  parse_advance(s);
  // Two-character operators need second advance
  if (op == OP_EQ || op == OP_NE || op == OP_LE || op == OP_GE ||
      op == OP_LSH || op == OP_RSH || op == OP_AND || op == OP_OR) {
    parse_advance(s);
  }
}

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
