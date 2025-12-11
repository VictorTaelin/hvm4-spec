fn Term parse_term_infix(Term f, PState *s, u32 depth, int min_prec) {
  Term t = 0;
  while (1) {
    t = parse_choice(f, s, depth, min_prec, (Parser[]){
      parse_postfix_red,   // ~>
      parse_postfix_cons,  // <>
      parse_postfix_eql,   // === (must be before opr which has ==)
      parse_postfix_and,   // .&.
      parse_postfix_or,    // .|.
      parse_postfix_opr,   // + - * / == != etc.
      parse_postfix_call,  // f(args)
      NULL});
    if (!t) return f;
    f = t;
  }
}
