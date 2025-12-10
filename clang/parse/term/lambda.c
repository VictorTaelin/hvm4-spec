fn Term parse_term(PState *s, u32 depth);
fn Term parse_term_era_lam(PState *s, u32 depth);
fn Term parse_term_mat_body(PState *s, u32 depth);
fn Term parse_term_lam_body(PState *s, u32 depth);

// λ{} | λ{..} | λ[&]x..
fn Term parse_term_lambda(PState *s, u32 depth) {
  if (!parse_match(s, "λ")) return 0;
  TermParser alts[] = {
    parse_term_era_lam,
    parse_term_mat_body,
    parse_term_lam_body,
    NULL
  };
  return parse_choice(s, depth, alts);
}
