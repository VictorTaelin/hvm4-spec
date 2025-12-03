// Collapser: Collapse a term, lifting SUPs to the top
// ====================================================
//
// This mirrors KolmoC's collapser architecture:
// - Descend through the term structure
// - Accumulate reconstruction context in a spine
// - When SUP found, apply spine to lift it to the top
// - Return term with all SUPs at the outermost level
//
// The key insight: when we find a SUP at any level, we:
// 1. Build a template for the CURRENT node (with vars for unprocessed children)
// 2. Recursively collapse children until we find a SUP or complete
// 3. If SUP found in child, inject it through our template
// 4. Return the result (which will be a SUP if any child had a SUP)

// Forward declaration
fn Term collapser_go(Term term);

// Main entry point: collapse a term
fn Term collapser(Term term) {
  return collapser_go(term);
}

// Internal: recursive collapse
// Returns the collapsed term. If a SUP is found anywhere inside,
// the result will have SUP at the top level.
fn Term collapser_go(Term term) {
  term = wnf(term);

  switch (term_tag(term)) {
    // Atomic terms: return as-is
    case ERA:
    case VAR:
    case REF:
    case NUM:
    case CO0:
    case CO1:
    case NAM:
    case DRY: {
      return term;
    }

    // SUP: already at top level, return as-is
    // (We don't descend into SUP branches - that's flatten's job)
    case SUP: {
      return term;
    }

    // LAM: special case (1 child)
    case LAM: {
      u64  lam_loc = term_val(term);
      Term body    = HEAP[lam_loc];

      // Recursively collapse the body
      Term body_collapsed = collapser_go(body);

      if (term_tag(body_collapsed) != SUP) {
        // No SUP in body - rebuild LAM with collapsed body
        HEAP[lam_loc] = body_collapsed;
        return term;
      }

      // SUP in body - lift it through LAM using template+inject
      // Template: λfV. (λk. fV)
      u64 outer_loc   = heap_alloc(1);
      HEAP[lam_loc]   = term_new(0, VAR, 0, outer_loc);
      Term inner_lam  = term_new(0, LAM, 0, lam_loc);
      HEAP[outer_loc] = inner_lam;
      Term template   = term_new(0, LAM, 0, outer_loc);

      Term args[1] = { body_collapsed };
      return collapse_inject(template, args, 1);
    }

    // Generic compound terms: CTR, APP, etc.
    default: {
      u32 ari = term_arity(term);
      u64 loc = term_val(term);

      if (ari == 0) {
        return term;
      }

      // Collapse each child
      Term collapsed[16];
      int  has_sup = 0;

      for (u32 i = 0; i < ari; i++) {
        collapsed[i] = collapser_go(HEAP[loc + i]);
        if (term_tag(collapsed[i]) == SUP) {
          has_sup = 1;
        }
      }

      if (!has_sup) {
        // No SUPs - rebuild node with collapsed children
        for (u32 i = 0; i < ari; i++) {
          HEAP[loc + i] = collapsed[i];
        }
        return term;
      }

      // Has SUP(s) - build template and inject
      // Template: λv0. λv1. ... Node(v0, v1, ...)

      // Allocate lambda body locations
      u64 lam_locs[16];
      for (u32 i = 0; i < ari; i++) {
        lam_locs[i] = heap_alloc(1);
      }

      // Build vars
      Term vars[16];
      for (u32 i = 0; i < ari; i++) {
        vars[i] = term_new(0, VAR, 0, lam_locs[i]);
      }

      // Build the node with vars
      Term node = term_new_(term_tag(term), term_ext(term), ari, vars);

      // Build nested lambdas from inside out
      Term body = node;
      for (int i = ari - 1; i >= 0; i--) {
        HEAP[lam_locs[i]] = body;
        body = term_new(0, LAM, 0, lam_locs[i]);
      }
      Term template = body;

      // Inject collapsed children
      return collapse_inject(template, collapsed, ari);
    }
  }
}
