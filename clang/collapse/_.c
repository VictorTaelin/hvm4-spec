fn Term collapse(Term term) {
  term = wnf(term);

  switch (term_tag(term)) {
    case ERA:
    case VAR:
    case REF:
    case NUM:
    case CO0:
    case CO1: {
      return term;
    }

    case SUP: {
      // Recursively collapse both branches, keep SUP at top
      u64  loc = term_val(term);
      Term a   = collapse(HEAP[loc + 0]);
      Term b   = collapse(HEAP[loc + 1]);
      return term_new_sup(term_ext(term), a, b);
    }

    case RED: {
      // For RED, collapse the rhs (g) side only
      u64  loc = term_val(term);
      return collapse(HEAP[loc + 1]);
    }

    case LAM: {
      // Haskell: fV <- fresh; f' <- collapse f; inject (Lam fV (Lam k (Var fV))) [f']
      u64  lam_loc = term_val(term);
      Term body    = HEAP[lam_loc];

      // Collapse the body
      Term body_collapsed = collapse(body);

      // Build template: λfV. (λk. fV)
      // - outer lambda binds fV, body location = outer_loc
      // - inner lambda binds k (original), body location = lam_loc
      // - inner body = Var fV = VAR(outer_loc)
      u64 outer_loc = heap_alloc(1);
      HEAP[lam_loc]   = term_new(0, VAR, 0, outer_loc);  // inner body = Var fV
      Term inner_lam  = term_new(0, LAM, 0, lam_loc);    // inner lambda (Lam k ...)
      HEAP[outer_loc] = inner_lam;                       // outer body = inner lambda
      Term template   = term_new(0, LAM, 0, outer_loc);  // outer lambda (Lam fV ...)

      Term args[1] = { body_collapsed };
      return collapse_inject(template, args, 1);
    }

    default: {
      // Generic case for APP, MAT, CTR, etc.
      // Template: λv0. λv1. ... T(Var v0, Var v1, ...)
      u32 ari = term_arity(term);
      u64 loc = term_val(term);

      if (ari == 0) {
        return term;
      }

      // Collapse all fields
      Term collapsed[16];
      for (u32 i = 0; i < ari; i++) {
        collapsed[i] = collapse(HEAP[loc + i]);
      }

      // Allocate lambda body locations (these are also the var binding points)
      u64 lam_locs[16];
      for (u32 i = 0; i < ari; i++) {
        lam_locs[i] = heap_alloc(1);
      }

      // Build vars pointing to their respective lambda body locations
      Term vars[16];
      for (u32 i = 0; i < ari; i++) {
        vars[i] = term_new(0, VAR, 0, lam_locs[i]);
      }

      // Build the node with vars using arity-generic constructor
      Term node = term_new_(term_tag(term), term_ext(term), ari, vars);

      // Build nested lambdas from inside out
      Term body = node;
      for (int32_t i = ari - 1; i >= 0; i--) {
        HEAP[lam_locs[i]] = body;
        body = term_new(0, LAM, 0, lam_locs[i]);
      }
      Term template = body;

      return collapse_inject(template, collapsed, ari);
    }
  }
}
