fn Term parse_term(PState *s, u32 depth);
fn Term parse_term_era_amp(PState *s, u32 depth);
fn Term parse_term_fork_body(PState *s, u32 depth);
fn Term parse_term_sup_body(PState *s, u32 depth);

// &{} | &[L|(L)]λx{a;b} | &[L|(L)]{a,b}
fn Term parse_term_amp(PState *s, u32 depth) {
  if (!parse_match(s, "&")) return 0;
  TermParser alts[] = {
    parse_term_era_amp,
    parse_term_fork_body,
    parse_term_sup_body,
    NULL
  };
  return parse_choice(s, depth, alts);
}
