// Collapse step: recursively searches for SUPs and lifts them to the top level.
// Stops when it finds a SUP (doesn't descend into SUP branches).
// Also strips RED nodes (keeping only RHS) and propagates ERA upward.
// Used by flatten() for lazy BFS enumeration of superposition branches.

fn Term collapse_step(Term term) {
  term = wnf(term);

  switch (term_tag(term)) {
    case ERA:
    case VAR:
    case REF:
    case NUM:
    case CO0:
    case CO1:
    case NAM: {
      return term;
    }

    case SUP: {
      // Found a SUP - return it immediately, don't descend into branches
      return term;
    }

    case RED: {
      // For RED, collapse the rhs (g) side only
      u64  loc = term_val(term);
      return collapse_step(HEAP[loc + 1]);
    }

    case LAM: {
      u64  lam_loc = term_val(term);
      Term body    = HEAP[lam_loc];

      // Recursively collapse the body to find SUPs
      Term body_collapsed = collapse_step(body);
      HEAP[lam_loc] = body_collapsed;

      // ERA propagation: if body is ERA, whole lambda is ERA
      if (term_tag(body_collapsed) == ERA) {
        return term_new_era();
      }

      if (term_tag(body_collapsed) != SUP) {
        // No SUP found in body - return the LAM unchanged
        return term;
      }

      // SUP in body - lift it using template+inject
      // Build template: λfV. (λk. fV)
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
      u32 ari = term_arity(term);
      u64 loc = term_val(term);

      if (ari == 0) {
        return term;
      }

      // Recursively collapse each subterm, looking for SUPs
      int  has_sup = 0;
      Term subterms[16];
      for (u32 i = 0; i < ari; i++) {
        subterms[i] = collapse_step(HEAP[loc + i]);
        HEAP[loc + i] = subterms[i];
        if (term_tag(subterms[i]) == SUP) {
          has_sup = 1;
        }
      }

      // ERA propagation: if any child is ERA, whole node is ERA
      for (u32 i = 0; i < ari; i++) {
        if (term_tag(subterms[i]) == ERA) {
          return term_new_era();
        }
      }

      if (!has_sup) {
        // No SUPs found anywhere - term is fully collapsed
        return term;
      }

      // Has SUP(s) - build template and inject
      // Template: λv0. λv1. ... T(Var v0, Var v1, ...)

      // Allocate lambda body locations
      u64 lam_locs[16];
      for (u32 i = 0; i < ari; i++) {
        lam_locs[i] = heap_alloc(1);
      }

      // Build vars pointing to their respective lambda body locations
      Term vars[16];
      for (u32 i = 0; i < ari; i++) {
        vars[i] = term_new(0, VAR, 0, lam_locs[i]);
      }

      // Build the node with vars
      Term node = term_new_(term_tag(term), term_ext(term), ari, vars);

      // Build nested lambdas from inside out
      Term body = node;
      for (int32_t i = ari - 1; i >= 0; i--) {
        HEAP[lam_locs[i]] = body;
        body = term_new(0, LAM, 0, lam_locs[i]);
      }
      Term template = body;

      return collapse_inject(template, subterms, ari);
    }
  }
}
