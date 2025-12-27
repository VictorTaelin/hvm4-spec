// Collapse step: lift the first SUP to the top, without descending into branches.
// RED keeps only RHS; ERA propagates upward. Used by collapse_flatten() for BFS.

fn Term collapse_step(Term term, u32 depth) {
  term = wnf(term);

  switch (term_tag(term)) {
    case ERA:
    case REF:
    case NUM:
    case NAM:
    case BJV:
    case BJ0:
    case BJ1: {
      return term;
    }

    case SUP: {
      return term;
    }

    case INC: {
      return term;
    }

    case RED: {
      u64 loc = term_val(term);
      return collapse_step(heap_read(loc + 1), depth);
    }

    case LAM: {
      u64  lam_loc = term_val(term);
      Term body    = heap_read(lam_loc);
      u32  level   = depth + 1;
      heap_subst_var(lam_loc, term_new(0, BJV, 0, level));
      Term body_collapsed = collapse_step(body, level);
      u64  body_loc = heap_alloc(1);
      heap_set(body_loc, body_collapsed);
      Term lam = term_new(0, LAM, level, body_loc);

      u8 body_tag = term_tag(body_collapsed);
      if (body_tag == ERA) {
        return term_new_era();
      }

      if (body_tag != SUP) {
        return lam;
      }

      u32  lab     = term_ext(body_collapsed);
      u64  sup_loc = term_val(body_collapsed);
      Term sup_a   = heap_read(sup_loc + 0);
      Term sup_b   = heap_read(sup_loc + 1);

      u64 loc0 = heap_alloc(1);
      u64 loc1 = heap_alloc(1);
      heap_set(loc0, sup_a);
      heap_set(loc1, sup_b);

      Term lam0 = term_new(0, LAM, level, loc0);
      Term lam1 = term_new(0, LAM, level, loc1);

      return term_new_sup(lab, lam0, lam1);
    }

    case DUP:
    case APP:
    case DRY:
    case MAT:
    case SWI:
    case USE:
    case OP2:
    case DSU:
    case DDU:
    case EQL:
    case AND:
    case OR:
    case UNS:
    case C00 ... C16: {
      u32 ari = term_arity(term);
      u32 loc = (u32)term_val(term);

      if (ari == 0) {
        return term;
      }

      int  sup_idx = -1;
      Term children[16];

      for (u32 i = 0; i < ari; i++) {
        Term child = heap_read(loc + i);
        children[i] = collapse_step(child, depth);
        if (children[i] != child) {
          heap_set(loc + i, children[i]);
        }

        if (term_tag(children[i]) == ERA) {
          return term_new_era();
        }

        if (sup_idx < 0 && term_tag(children[i]) == SUP) {
          sup_idx = i;
        }
      }

      if (sup_idx < 0) {
        return term;
      }

      Term sup     = children[sup_idx];
      u32  lab     = term_ext(sup);
      u64  sup_loc = term_val(sup);
      Term sup_a   = heap_read(sup_loc + 0);
      Term sup_b   = heap_read(sup_loc + 1);

      Term args0[16], args1[16];

      for (u32 i = 0; i < ari; i++) {
        if ((int)i == sup_idx) {
          args0[i] = sup_a;
          args1[i] = sup_b;
        } else {
          Copy c = term_clone(lab, children[i]);
          args0[i] = c.k0;
          args1[i] = c.k1;
        }
      }

      Term node0 = term_new_at(loc, term_tag(term), term_ext(term), ari, args0);
      Term node1 = term_new_(term_tag(term), term_ext(term), ari, args1);

      return term_new_sup(lab, node0, node1);
    }

    default: {
      return term;
    }
  }
}
