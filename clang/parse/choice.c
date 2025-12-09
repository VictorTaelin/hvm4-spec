typedef Term (*TermParser)(PState *s, u32 depth);
fn Term parse_choice(PState *s, u32 depth, TermParser const *alts) {
  for (size_t i = 0; alts[i] != NULL; i++) {
    // save initial parser state
    u32 pos = s->pos;
    u32 len = s->len;
    u32 line = s->line;
    u32 col = s-> col;

    // try parser, return if succesful
    Term t = alts[i](s, depth);
    if (t) return t;

    // restore initial parser state
    s->pos = pos;
    s->len = len;
    s->line = line;
    s->col = col;
  }
  return 0;
}
