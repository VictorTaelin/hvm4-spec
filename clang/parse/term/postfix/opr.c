fn Term parse_term_atom(PState *s, u32 depth);
fn Term parse_term(Term f, PState *s, u32 depth, int min_prec);
fn int parse_term_opr_peek(PState *s);
fn void parse_term_opr_consume(PState *s, int op);
fn int parse_term_opr_prec(int op);

fn Term parse_postfix_opr(Term f, PState *s, u32 depth, int min_prec) {
  int op = parse_term_opr_peek(s);
  if (op < 0) return 0;
  if (parse_term_opr_prec(op) < min_prec) return 0;
  parse_term_opr_consume(s, op);
  int prec = parse_term_opr_prec(op);
  // Use prec+1 for RHS to make infix ops left-associative:
  // if we used prec, another op of the same precedence could attach on the RHS.
  // For right-associative operators (like <>), parse with prec instead.
  Term rhs = parse_term(NONE, s, depth, prec + 1);
  return term_new_op2(op, f, rhs);
}
