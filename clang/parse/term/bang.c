fn Term parse_term(PState *s, u32 depth);
fn Term parse_term_uns_body(PState *s, u32 depth);
fn Term parse_term_strict_let_body(PState *s, u32 depth);
fn Term parse_term_let_body(PState *s, u32 depth);
fn Term parse_term_dup_stmt_body(PState *s, u32 depth);

// ! ${x,k};b | !!x=v;b | ![&]x=v;b | ![&]x&[L|(L)]=v;b
fn Term parse_term_bang(PState *s, u32 depth) {
  if (!parse_match(s, "!")) return 0;
  parse_skip(s);
  TermParser alts[] = {
    parse_term_uns_body,
    parse_term_strict_let_body,
    parse_term_let_body,
    parse_term_dup_stmt_body,
    NULL
  };
  return parse_choice(s, depth, alts);
}
