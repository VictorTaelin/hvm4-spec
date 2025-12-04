// Collapser: Spine-based SUP lifting (KolmoC-style)
// ==================================================
//
// This collapser uses a spine to accumulate reconstruction context as we
// descend into terms. When we hit a SUP, we clone the spine (and remaining
// work) with the SUP's label, ensuring proper DUP-SUP annihilation.
//
// Key differences from the old template+inject approach:
// 1. Single unified function (no separate collapse/collapse_step/inject)
// 2. Explicit spine tracks reconstruction, clonable as a unit
// 3. When entering LAM, we mark the binding with SUB|NAM (as DEP marker)
// 4. SUP handling clones the entire context with the SUP's label

// ============================================================
// Spine Data Structure
// ============================================================

#define SPINE_MAX 256
#define WORK_MAX  4096

// A spine frame: one node being reconstructed
typedef struct {
  u8   tag;           // Node type (LAM, APP, CTR, etc.)
  u32  lab;           // Label/extension
  u64  loc;           // Original heap location (for LAM: lam_loc)
  u8   arity;         // Total children expected
  u8   filled;        // How many children filled so far
  Term children[16];  // Filled children
  Term saved;         // Saved heap value (for LAM: original binding)
} Frame;

// The spine: stack of frames being reconstructed
typedef struct {
  Frame frames[SPINE_MAX];
  u32   depth;
  u32   lam_depth;  // Count of LAM frames (for variable naming)
} Spine;

// ============================================================
// Spine Operations
// ============================================================

fn void spine_init(Spine* s) {
  s->depth = 0;
  s->lam_depth = 0;
}

fn void spine_push(Spine* s, u8 tag, u32 lab, u64 loc, u8 arity) {
  if (s->depth >= SPINE_MAX) {
    fprintf(stderr, "spine overflow\n");
    exit(1);
  }
  Frame* f = &s->frames[s->depth++];
  f->tag = tag;
  f->lab = lab;
  f->loc = loc;
  f->arity = arity;
  f->filled = 0;
  if (tag == LAM) {
    s->lam_depth++;
  }
}

fn void spine_fill(Spine* s, Term val) {
  if (s->depth == 0) {
    fprintf(stderr, "spine_fill on empty spine\n");
    exit(1);
  }
  Frame* f = &s->frames[s->depth - 1];
  if (f->filled >= f->arity) {
    fprintf(stderr, "spine_fill overflow\n");
    exit(1);
  }
  f->children[f->filled++] = val;
}

fn int spine_complete(Spine* s) {
  if (s->depth == 0) return 0;
  Frame* f = &s->frames[s->depth - 1];
  return f->filled >= f->arity;
}

fn Term spine_pop(Spine* s) {
  if (s->depth == 0) {
    fprintf(stderr, "spine_pop on empty spine\n");
    exit(1);
  }
  Frame* f = &s->frames[--s->depth];

  // Reconstruct node based on tag
  if (f->tag == LAM) {
    s->lam_depth--;
    // Restore original heap value (undo the NAM marking)
    HEAP[f->loc] = f->saved;
    // For LAM: create new lambda with the processed body
    u64 new_loc = heap_alloc(1);
    HEAP[new_loc] = f->children[0];
    return term_new(0, LAM, f->lab, new_loc);
  } else {
    // For other nodes: use generic constructor
    return term_new_(f->tag, f->lab, f->arity, f->children);
  }
}

// Forward declaration
fn void spine_clone(u32 lab, Spine* src, Spine* dst0, Spine* dst1);

// Reconstruct a term by wrapping it in all spine frames (bottom-up)
// If inner is a SUP, clone the spine and recurse to lift SUP to top level
fn Term spine_reconstruct(Spine* s, Term inner) {
  // If inner is SUP, we need to lift it: clone spine and wrap each branch
  if (term_tag(inner) == SUP) {
    u32 lab = term_ext(inner);
    u64 loc = term_val(inner);
    Spine s0, s1;
    spine_clone(lab, s, &s0, &s1);
    Term r0 = spine_reconstruct(&s0, HEAP[loc + 0]);
    Term r1 = spine_reconstruct(&s1, HEAP[loc + 1]);
    return term_new_sup(lab, r0, r1);
  }

  // Apply frames from top to bottom (innermost to outermost)
  for (int i = s->depth - 1; i >= 0; i--) {
    Frame* f = &s->frames[i];

    if (f->tag == LAM) {
      // For LAM: create new lambda with inner as body
      u64 new_loc = heap_alloc(1);
      HEAP[new_loc] = inner;
      inner = term_new(0, LAM, f->lab, new_loc);
    } else {
      // For other nodes: reconstruct with filled children + inner
      // The inner term goes in the next unfilled position
      Term children[16];
      for (u32 j = 0; j < f->filled; j++) {
        children[j] = f->children[j];
      }
      children[f->filled] = inner;
      // Remaining children (if any) would need to come from work list
      // For now, assume inner is the last child needed
      inner = term_new_(f->tag, f->lab, f->filled + 1, children);
    }
  }
  return inner;
}

// Clone a spine with a given label for DUP-SUP interaction
fn void spine_clone(u32 lab, Spine* src, Spine* dst0, Spine* dst1) {
  dst0->depth = src->depth;
  dst1->depth = src->depth;
  dst0->lam_depth = src->lam_depth;
  dst1->lam_depth = src->lam_depth;

  for (u32 i = 0; i < src->depth; i++) {
    Frame* sf = &src->frames[i];

    // Copy frame structure
    dst0->frames[i] = *sf;
    dst1->frames[i] = *sf;

    // Clone filled children with the label
    for (u32 j = 0; j < sf->filled; j++) {
      Copy c = term_clone(lab, sf->children[j]);
      dst0->frames[i].children[j] = c.k0;
      dst1->frames[i].children[j] = c.k1;
    }
  }
}

// ============================================================
// Collapser Implementation
// ============================================================

fn Term collapser_go(Spine* spine, Term* work, u32 work_count);

// Main entry point
fn Term collapser(Term term) {
  Spine spine;
  spine_init(&spine);

  Term work[WORK_MAX];
  u32 work_count = 1;
  work[0] = term;

  return collapser_go(&spine, work, work_count);
}

// Recursive worker (called with copied arrays for SUP branches)
fn Term collapser_go(Spine* spine, Term* work, u32 work_count) {

  while (work_count > 0) {
    Term term = work[--work_count];
    term = wnf(term);

    u8  tag = term_tag(term);
    u32 ext = term_ext(term);
    u64 loc = term_val(term);

    switch (tag) {
      // --------------------------------------------------------
      // SUP: Clone context, wrap branches, return SUP
      // --------------------------------------------------------
      case SUP: {
        // If spine is empty and no more work, return SUP as-is
        // This allows lazy enumeration via flatten's BFS queue
        if (spine->depth == 0 && work_count == 0) {
          return term;
        }

        // Get SUP branches
        Term b0 = HEAP[loc + 0];
        Term b1 = HEAP[loc + 1];

        // Clone spine with SUP's label
        Spine s0, s1;
        spine_clone(ext, spine, &s0, &s1);

        // If no remaining work, wrap branches directly (avoids infinite recursion)
        if (work_count == 0) {
          Term r0 = spine_reconstruct(&s0, b0);
          Term r1 = spine_reconstruct(&s1, b1);
          return term_new_sup(ext, r0, r1);
        }

        // Clone remaining work items with SUP's label
        Term w0[WORK_MAX], w1[WORK_MAX];
        for (u32 i = 0; i < work_count; i++) {
          Copy c = term_clone(ext, work[i]);
          w0[i] = c.k0;
          w1[i] = c.k1;
        }

        // Add SUP branches to respective work lists
        w0[work_count] = b0;
        w1[work_count] = b1;
        u32 wc = work_count + 1;

        // Recursively process both branches
        Term r0 = collapser_go(&s0, w0, wc);
        Term r1 = collapser_go(&s1, w1, wc);

        return term_new_sup(ext, r0, r1);
      }

      // --------------------------------------------------------
      // LAM: Mark binding with NAM (as DEP), push frame, add body
      // --------------------------------------------------------
      case LAM: {
        Term body = HEAP[loc];

        // Use lam_depth + 1 for variable naming (matches print_term)
        u32 var_name = spine->lam_depth + 1;

        // Save original and mark binding with SUB|NAM (DEP marker)
        // The saved value will be restored when the frame is popped
        Term saved = HEAP[loc];
        HEAP[loc] = term_new(1, NAM, var_name, 0);

        // Push LAM frame to spine (store saved value for restoration)
        spine_push(spine, LAM, var_name, loc, 1);
        spine->frames[spine->depth - 1].saved = saved;

        // Add body to work
        work[work_count++] = body;
        break;
      }

      // --------------------------------------------------------
      // RED: Skip the guard, continue with the body
      // --------------------------------------------------------
      case RED: {
        // RED(f, g): collapse g (the body), skip f (the guard)
        work[work_count++] = HEAP[loc + 1];
        break;
      }

      // --------------------------------------------------------
      // Compound nodes: Push frame, add children in reverse order
      // --------------------------------------------------------
      case APP:
      case DRY:
      case MAT:
      case SWI:
      case OP2:
      case EQL: {
        u32 arity = term_arity(term);
        spine_push(spine, tag, ext, loc, arity);

        // Add children in reverse order (so first child is processed first)
        for (int i = arity - 1; i >= 0; i--) {
          work[work_count++] = HEAP[loc + i];
        }
        break;
      }

      // CTR nodes (C00-C16)
      case C00: case C01: case C02: case C03:
      case C04: case C05: case C06: case C07:
      case C08: case C09: case C10: case C11:
      case C12: case C13: case C14: case C15:
      case C16: {
        u32 arity = term_arity(term);

        if (arity == 0) {
          // 0-arity: treat as atom
          goto handle_atom;
        }

        spine_push(spine, tag, ext, loc, arity);

        for (int i = arity - 1; i >= 0; i--) {
          work[work_count++] = HEAP[loc + i];
        }
        break;
      }

      // --------------------------------------------------------
      // ERA: Propagate erasure - destroys the entire context
      // --------------------------------------------------------
      case ERA: {
        // ERA infects everything - return ERA immediately
        return term_new_era();
      }

      // --------------------------------------------------------
      // Atoms: Fill into spine
      // --------------------------------------------------------
      case NUM:
      case REF:
      case NAM:
      case CO0:
      case CO1:
      case VAR:
      default: {
        handle_atom:
        if (spine->depth == 0) {
          // No context - return the atom directly
          if (work_count == 0) {
            return term;
          }
          // This shouldn't happen with well-formed terms
          fprintf(stderr, "collapser: atom with empty spine but non-empty work\n");
          return term;
        }

        spine_fill(spine, term);
        break;
      }
    }

    // Complete finished frames
    while (spine_complete(spine)) {
      Term node = spine_pop(spine);

      if (spine->depth == 0) {
        // Root frame completed
        if (work_count == 0) {
          return node;
        }
        // More work but no spine - shouldn't happen
        fprintf(stderr, "collapser: completed root but work remains\n");
        return node;
      }

      // Fill completed node into parent frame
      spine_fill(spine, node);
    }
  }

  // Work list empty - should have returned by now
  if (spine->depth == 0) {
    fprintf(stderr, "collapser: empty work and empty spine\n");
    return term_new_era();
  }

  fprintf(stderr, "collapser: empty work but non-empty spine\n");
  return term_new_era();
}
