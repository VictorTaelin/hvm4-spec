fn Term parse_term(PState *s, u32 depth);

// Map nick-encoded primitive names to operation codes
fn int parse_prim_to_op(u32 nam) {
  switch (nam) {
    case NAM_ADD: return OP_ADD;
    case NAM_SUB: return OP_SUB;
    case NAM_MUL: return OP_MUL;
    case NAM_DIV: return OP_DIV;
    case NAM_MOD: return OP_MOD;
    case NAM_AND: return OP_AND;
    case NAM_OR:  return OP_OR;
    case NAM_XOR: return OP_XOR;
    case NAM_LSH: return OP_LSH;
    case NAM_RSH: return OP_RSH;
    case NAM_NOT: return OP_NOT;
    case NAM_EQ:  return OP_EQ;
    case NAM_NE:  return OP_NE;
    case NAM_LT:  return OP_LT;
    case NAM_LE:  return OP_LE;
    case NAM_GT:  return OP_GT;
    case NAM_GE:  return OP_GE;
    default:      return -1;
  }
}

fn Term parse_term_pri(PState *s, u32 depth) {
  u32  nam = parse_name(s);
  parse_consume(s, "(");
  Term args[16];
  u32  cnt = parse_term_args(s, depth, args, 16);

  // Check if it's a numeric operation
  int op = parse_prim_to_op(nam);
  if (op >= 0) {
    if (op == OP_NOT) {
      // Unary: @@not(x) -> Op2(OP_NOT, 0, x)
      if (cnt != 1) {
        fprintf(stderr, "@@not expects 1 argument\n");
        exit(1);
      }
      return term_new_op2(OP_NOT, term_new_num(0), args[0]);
    } else {
      // Binary: @@add(x, y) -> Op2(OP_ADD, x, y)
      if (cnt != 2) {
        fprintf(stderr, "binary op expects 2 arguments\n");
        exit(1);
      }
      return term_new_op2(op, args[0], args[1]);
    }
  }

  // Handle @@sup(lab, a, b) -> DYS
  if (nam == NAM_SUP) {
    if (cnt != 3) {
      fprintf(stderr, "@@sup expects 3 arguments\n");
      exit(1);
    }
    return term_new_dys(args[0], args[1], args[2]);
  }

  // Handle @@dup(lab, val, fun) -> DYD
  if (nam == NAM_DUP) {
    if (cnt != 3) {
      fprintf(stderr, "@@dup expects 3 arguments\n");
      exit(1);
    }
    return term_new_dyd(args[0], args[1], args[2]);
  }

  fprintf(stderr, "unknown primitive: ");
  print_name(stderr, nam);
  fprintf(stderr, "\n");
  exit(1);
}
