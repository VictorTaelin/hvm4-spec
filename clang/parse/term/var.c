fn Term parse_term_var(PState *s, u32 depth) {
  parse_skip(s);
  u32 nam = parse_name(s);
  int lvl;
  u32 lab;
  u32 cloned;
  parse_bind_lookup(nam, &lvl, &lab, &cloned);
  parse_skip(s);
  int side = parse_match(s, "₀") ? 0 : parse_match(s, "₁") ? 1 : -1;
  // Fork mode: auto-select side for dup variables
  if (side == -1 && lab != 0 && PARSE_FORK_SIDE >= 0) side = PARSE_FORK_SIDE;
  parse_skip(s);
  // Error if variable is not found in bindings
  if (lvl < 0) {
    parse_error_var("- undefined variable '%s'\n", nam);
  }
  // Handle dynamic dup binding (lab=0xFFFFFF marker)
  // For dynamic dup, X₀ and X₁ become BJV references to nested lambdas
  // Structure: @dup(lab, val, λ_.λ_.body) where:
  //   - outer lambda is at level L (receives dup side 0 via @dup)
  //   - inner lambda is at level L+1 (receives dup side 1 via @dup)
  if (lab == 0xFFFFFF) {
    if (side == 0) {
      parse_bind_inc_side(nam, 0);  // Track X₀ uses for cloned dynamic dup
      return term_new(0, BJV, 0, (u32)lvl);  // X₀ → outer lambda
    } else if (side == 1) {
      parse_bind_inc_side(nam, 1);  // Track X₁ uses for cloned dynamic dup
      return term_new(0, BJV, 0, (u32)lvl + 1);  // X₁ → inner lambda
    } else {
      parse_error_var("- dynamic dup variable '%s' requires subscript ₀ or ₁\n", nam);
    }
  }
  // If dup-bound variable used without subscript (and not in fork mode),
  // try to find an outer non-dup binding with capacity
  if (lab != 0 && side == -1) {
    if (parse_bind_lookup_skip_dup(nam, &lvl, &lab, &cloned)) {
      // Found outer binding with capacity - use it as BJV
      return term_new(0, BJV, 0, (u32)lvl);
    } else {
      parse_error_var("- dup variable '%s' requires subscript ₀ or ₁\n", nam);
    }
  }
  // Track per-side uses for dup bindings and check affinity
  if (lab != 0 && side >= 0) {
    u32 prev_uses = parse_bind_inc_side(nam, side);
    if (!cloned && prev_uses > 0) {
      parse_error_affine_side(nam, side, prev_uses + 1);
    }
  }
  u32 val = (u32)lvl;
  u8  tag = (side == 0) ? BJ0 : (side == 1) ? BJ1 : BJV;
  return term_new(0, tag, lab, val);
}
