// HVM4 Runtime Implementation in Metal
// =====================================
//
// This file implements the HVM4 compute kernel for GPU execution.
// It follows the same structure as hvm4.c but adapted for Metal Shading Language.
//
// Term Pointer Layout (64-bit)
// ----------------------------
// | SUB  (1 bit)   ::= marks heap slot as containing a substitution
// | TAG  (7 bits)  ::= constructor variant (APP, LAM, SUP, etc.)
// | EXT  (24 bits) ::= dup label, ctr name, or ref name
// | VAL  (32 bits) ::= heap address or unboxed value

#include <metal_stdlib>
using namespace metal;

// Types
// =====

typedef uint64_t Term;

struct Copy {
  Term k0;
  Term k1;
};

// Tags
// ====

constant uint8_t NAM = 0;
constant uint8_t DRY = 1;
constant uint8_t REF = 2;
constant uint8_t ALO = 3;
constant uint8_t ERA = 4;
constant uint8_t CO0 = 5;
constant uint8_t CO1 = 6;
constant uint8_t VAR = 7;
constant uint8_t LAM = 8;
constant uint8_t APP = 9;
constant uint8_t SUP = 10;
constant uint8_t DUP = 11;
constant uint8_t MAT = 12;
constant uint8_t CTR = 13;
constant uint8_t CTR_MAX_ARI = 16;

// Bit Layout
// ==========

constant uint64_t SUB_SHIFT = 63;
constant uint64_t TAG_SHIFT = 56;
constant uint64_t EXT_SHIFT = 32;

constant uint64_t SUB_MASK = 0x1;
constant uint64_t TAG_MASK = 0x7F;
constant uint64_t EXT_MASK = 0xFFFFFF;
constant uint64_t VAL_MASK = 0xFFFFFFFF;

// Term Helpers
// ============

inline Term new_term(uint8_t sub, uint8_t tag, uint32_t ext, uint32_t val) {
  return (uint64_t(sub) << SUB_SHIFT)
       | (uint64_t(tag & uint8_t(TAG_MASK)) << TAG_SHIFT)
       | (uint64_t(ext & uint32_t(EXT_MASK)) << EXT_SHIFT)
       | uint64_t(val & uint32_t(VAL_MASK));
}

inline uint8_t sub_of(Term t) {
  return uint8_t((t >> SUB_SHIFT) & SUB_MASK);
}

inline uint8_t tag(Term t) {
  return uint8_t((t >> TAG_SHIFT) & TAG_MASK);
}

inline uint32_t ext(Term t) {
  return uint32_t((t >> EXT_SHIFT) & EXT_MASK);
}

inline uint32_t val(Term t) {
  return uint32_t(t & VAL_MASK);
}

inline uint32_t arity_of(Term t) {
  uint8_t tg = tag(t);
  if (tg == LAM) { return 1; }
  if (tg == APP || tg == SUP || tg == DUP || tg == MAT || tg == DRY) { return 2; }
  if (tg >= CTR && tg <= CTR + CTR_MAX_ARI) { return uint32_t(tg - CTR); }
  return 0;
}

inline Term mark_sub(Term t) {
  return t | (uint64_t(1) << SUB_SHIFT);
}

inline Term clear_sub(Term t) {
  return t & ~(SUB_MASK << SUB_SHIFT);
}

// Term Constructors
// =================

inline Term Var(uint32_t loc) {
  return new_term(0, VAR, 0, loc);
}

inline Term Era() {
  return new_term(0, ERA, 0, 0);
}

inline Term Co0(uint32_t lab, uint32_t loc) {
  return new_term(0, CO0, lab, loc);
}

inline Term Co1(uint32_t lab, uint32_t loc) {
  return new_term(0, CO1, lab, loc);
}

inline Term Nam(uint32_t nam) {
  return new_term(0, NAM, 0, nam);
}

// Cloning
// =======

inline Copy clone_at(uint32_t loc, uint32_t lab) {
  return Copy{ Co0(lab, loc), Co1(lab, loc) };
}

// Substitution Helpers
// --------------------

inline void subst_var(device Term* heap, uint32_t loc, Term v) {
  heap[loc] = mark_sub(v);
}

inline Term subst_cop(device Term* heap, uint8_t side, uint32_t loc, Term r0, Term r1) {
  heap[loc] = mark_sub(side == 0 ? r1 : r0);
  return side == 0 ? r0 : r1;
}

// Runtime State
// =============

struct State {
  uint32_t alloc;
  uint64_t itrs;
};

// Heap Allocation
// ===============

inline uint32_t heap_alloc(thread State& st, uint32_t size) {
  uint32_t at = st.alloc;
  st.alloc += size;
  return at;
}

// Term Constructors (with allocation)
// ===================================

inline Term LamAt(device Term* heap, uint32_t loc, Term bod) {
  heap[loc] = bod;
  return new_term(0, LAM, 0, loc);
}

inline Term Lam(device Term* heap, thread State& st, Term bod) {
  uint32_t loc = heap_alloc(st, 1);
  heap[loc] = bod;
  return new_term(0, LAM, 0, loc);
}

inline Term AppAt(device Term* heap, uint32_t loc, Term fun, Term arg) {
  heap[loc + 0] = fun;
  heap[loc + 1] = arg;
  return new_term(0, APP, 0, loc);
}

inline Term App(device Term* heap, thread State& st, Term fun, Term arg) {
  uint32_t loc = heap_alloc(st, 2);
  heap[loc + 0] = fun;
  heap[loc + 1] = arg;
  return new_term(0, APP, 0, loc);
}

inline Term SupAt(device Term* heap, uint32_t loc, uint32_t lab, Term tm0, Term tm1) {
  heap[loc + 0] = tm0;
  heap[loc + 1] = tm1;
  return new_term(0, SUP, lab, loc);
}

inline Term Sup(device Term* heap, thread State& st, uint32_t lab, Term tm0, Term tm1) {
  uint32_t loc = heap_alloc(st, 2);
  heap[loc + 0] = tm0;
  heap[loc + 1] = tm1;
  return new_term(0, SUP, lab, loc);
}

inline Term DryAt(device Term* heap, uint32_t loc, Term tm0, Term tm1) {
  heap[loc + 0] = tm0;
  heap[loc + 1] = tm1;
  return new_term(0, DRY, 0, loc);
}

inline Term Dry(device Term* heap, thread State& st, Term tm0, Term tm1) {
  uint32_t loc = heap_alloc(st, 2);
  heap[loc + 0] = tm0;
  heap[loc + 1] = tm1;
  return new_term(0, DRY, 0, loc);
}

inline Term DupAt(device Term* heap, uint32_t loc, uint32_t lab, Term v, Term bod) {
  heap[loc + 0] = v;
  heap[loc + 1] = bod;
  return new_term(0, DUP, lab, loc);
}

inline Term Dup(device Term* heap, thread State& st, uint32_t lab, Term v, Term bod) {
  uint32_t loc = heap_alloc(st, 2);
  heap[loc + 0] = v;
  heap[loc + 1] = bod;
  return new_term(0, DUP, lab, loc);
}

// Clone with allocation
// =====================

inline Copy clone(device Term* heap, thread State& st, uint32_t lab, Term v) {
  uint32_t loc = heap_alloc(st, 1);
  heap[loc] = v;
  return clone_at(loc, lab);
}

// Beta Interactions
// =================

inline Term app_era(thread State& st) {
  st.itrs++;
  return Era();
}

inline Term app_stuck(device Term* heap, thread State& st, Term fun, Term arg) {
  st.itrs++;
  return Dry(heap, st, fun, arg);
}

inline Term app_lam(device Term* heap, thread State& st, Term lam, Term arg) {
  st.itrs++;
  uint32_t loc  = val(lam);
  Term     body = heap[loc];
  subst_var(heap, loc, arg);
  return body;
}

inline Term app_sup(device Term* heap, thread State& st, Term app, Term sup) {
  st.itrs++;
  uint32_t app_loc = val(app);
  uint32_t sup_loc = val(sup);
  uint32_t lab     = ext(sup);
  Term     arg     = heap[app_loc + 1];
  Term     tm1     = heap[sup_loc + 1];
  uint32_t loc     = heap_alloc(st, 3);
  heap[loc + 2]    = arg;
  Copy D = clone_at(loc + 2, lab);
  heap[sup_loc + 1] = D.k0;
  Term ap0 = new_term(0, APP, 0, sup_loc);
  Term ap1 = AppAt(heap, loc, tm1, D.k1);
  return SupAt(heap, app_loc, lab, ap0, ap1);
}

// Match Interactions
// ==================

inline Term app_mat_sup(device Term* heap, thread State& st, Term mat, Term sup) {
  st.itrs++;
  uint32_t lab = ext(sup);
  Copy M       = clone(heap, st, lab, mat);
  uint32_t loc = val(sup);
  Term a       = heap[loc + 0];
  Term b       = heap[loc + 1];
  return Sup(heap, st, lab, App(heap, st, M.k0, a), App(heap, st, M.k1, b));
}

inline Term app_mat_ctr(device Term* heap, thread State& st, Term mat, Term ctr) {
  st.itrs++;
  uint32_t ari = uint32_t(tag(ctr) - CTR);
  if (ext(mat) == ext(ctr)) {
    Term res = heap[val(mat)];
    for (uint32_t i = 0; i < ari; i++) {
      res = App(heap, st, res, heap[val(ctr) + i]);
    }
    return res;
  } else {
    return App(heap, st, heap[val(mat) + 1], ctr);
  }
}

// Dup Interactions
// ================

inline Term dup_lam(device Term* heap, thread State& st, uint32_t lab, uint32_t loc, uint8_t side, Term lam) {
  st.itrs++;
  uint32_t lam_loc = val(lam);
  Term     bod     = heap[lam_loc];
  uint32_t a       = heap_alloc(st, 5);
  heap[a + 4]      = bod;
  Copy B  = clone_at(a + 4, lab);
  Term su = SupAt(heap, a + 2, lab, Var(a), Var(a + 1));
  Term l0 = LamAt(heap, a + 0, B.k0);
  Term l1 = LamAt(heap, a + 1, B.k1);
  subst_var(heap, lam_loc, su);
  return subst_cop(heap, side, loc, l0, l1);
}

inline Term dup_sup(device Term* heap, thread State& st, uint32_t lab, uint32_t loc, uint8_t side, Term sup) {
  st.itrs++;
  uint32_t sup_loc = val(sup);
  uint32_t sup_lab = ext(sup);
  if (lab == sup_lab) {
    Term tm0 = heap[sup_loc + 0];
    Term tm1 = heap[sup_loc + 1];
    return subst_cop(heap, side, loc, tm0, tm1);
  } else {
    Copy A      = clone_at(sup_loc + 0, lab);
    Copy B      = clone_at(sup_loc + 1, lab);
    uint32_t a  = heap_alloc(st, 4);
    Term s0     = SupAt(heap, a + 0, sup_lab, A.k0, B.k0);
    Term s1     = SupAt(heap, a + 2, sup_lab, A.k1, B.k1);
    return subst_cop(heap, side, loc, s0, s1);
  }
}

inline Term dup_node(device Term* heap, thread State& st, uint32_t lab, uint32_t loc, uint8_t side, Term term) {
  st.itrs++;
  uint32_t ari = arity_of(term);
  if (ari == 0) {
    subst_var(heap, loc, term);
    return term;
  }
  uint32_t t_loc = val(term);
  uint32_t t_ext = ext(term);
  uint8_t  t_tag = tag(term);

  // For arity 1 (LAM)
  if (ari == 1) {
    Copy A = clone(heap, st, lab, heap[t_loc]);
    uint32_t loc0 = heap_alloc(st, 1);
    uint32_t loc1 = heap_alloc(st, 1);
    heap[loc0] = A.k0;
    heap[loc1] = A.k1;
    Term r0 = new_term(0, t_tag, t_ext, loc0);
    Term r1 = new_term(0, t_tag, t_ext, loc1);
    return subst_cop(heap, side, loc, r0, r1);
  }

  // For arity 2 (APP, SUP, DUP, MAT, DRY)
  if (ari == 2) {
    Copy A = clone(heap, st, lab, heap[t_loc + 0]);
    Copy B = clone(heap, st, lab, heap[t_loc + 1]);
    uint32_t loc0 = heap_alloc(st, 2);
    uint32_t loc1 = heap_alloc(st, 2);
    heap[loc0 + 0] = A.k0;
    heap[loc0 + 1] = B.k0;
    heap[loc1 + 0] = A.k1;
    heap[loc1 + 1] = B.k1;
    Term r0 = new_term(0, t_tag, t_ext, loc0);
    Term r1 = new_term(0, t_tag, t_ext, loc1);
    return subst_cop(heap, side, loc, r0, r1);
  }

  // For CTR with higher arity (up to 16)
  Copy copies[16];
  for (uint32_t i = 0; i < ari; i++) {
    copies[i] = clone(heap, st, lab, heap[t_loc + i]);
  }
  uint32_t loc0 = heap_alloc(st, ari);
  uint32_t loc1 = heap_alloc(st, ari);
  for (uint32_t i = 0; i < ari; i++) {
    heap[loc0 + i] = copies[i].k0;
    heap[loc1 + i] = copies[i].k1;
  }
  Term r0 = new_term(0, t_tag, t_ext, loc0);
  Term r1 = new_term(0, t_tag, t_ext, loc1);
  return subst_cop(heap, side, loc, r0, r1);
}

// Alloc Helpers
// =============

inline uint32_t bind_at(device Term* heap, uint32_t ls, uint32_t idx) {
  for (uint32_t i = 0; i < idx && ls != 0; i++) {
    ls = uint32_t(heap[ls] & VAL_MASK);
  }
  return (ls != 0) ? uint32_t(heap[ls] >> 32) : 0;
}

inline uint32_t make_bind(device Term* heap, thread State& st, uint32_t tail, uint32_t loc) {
  uint32_t entry = heap_alloc(st, 1);
  heap[entry] = (uint64_t(loc) << 32) | uint64_t(tail);
  return entry;
}

inline Term make_alo(device Term* heap, thread State& st, uint32_t ls_loc, uint32_t tm_loc) {
  uint32_t loc = heap_alloc(st, 1);
  heap[loc] = (uint64_t(ls_loc) << 32) | uint64_t(tm_loc);
  return new_term(0, ALO, 0, loc);
}

// Alloc Interactions
// ==================

inline Term alo_var(device Term* heap, uint32_t ls_loc, uint32_t idx) {
  uint32_t bind = bind_at(heap, ls_loc, idx);
  return bind != 0 ? Var(bind) : new_term(0, VAR, 0, idx);
}

inline Term alo_cop(device Term* heap, uint32_t ls_loc, uint32_t idx, uint32_t lab, uint8_t side) {
  uint32_t bind = bind_at(heap, ls_loc, idx);
  uint8_t  tg   = side == 0 ? CO0 : CO1;
  return bind != 0 ? new_term(0, tg, lab, bind) : new_term(0, tg, lab, idx);
}

inline Term alo_lam(device Term* heap, thread State& st, uint32_t ls_loc, uint32_t book_body_loc) {
  uint32_t lam_body = heap_alloc(st, 1);
  uint32_t new_bind = make_bind(heap, st, ls_loc, lam_body);
  heap[lam_body]    = make_alo(heap, st, new_bind, book_body_loc);
  return new_term(0, LAM, 0, lam_body);
}

inline Term alo_dup(device Term* heap, thread State& st, uint32_t ls_loc, uint32_t book_loc, uint32_t lab) {
  uint32_t dup_val  = heap_alloc(st, 1);
  uint32_t new_bind = make_bind(heap, st, ls_loc, dup_val);
  heap[dup_val]     = make_alo(heap, st, ls_loc, book_loc);
  return Dup(heap, st, lab, make_alo(heap, st, ls_loc, book_loc), make_alo(heap, st, new_bind, book_loc + 1));
}

inline Term alo_node(device Term* heap, thread State& st, uint32_t ls_loc, uint32_t loc, uint8_t tg, uint32_t ex, uint32_t ari) {
  uint32_t new_loc = heap_alloc(st, ari);
  for (uint32_t i = 0; i < ari; i++) {
    heap[new_loc + i] = make_alo(heap, st, ls_loc, loc + i);
  }
  return new_term(0, tg, ex, new_loc);
}

// WNF
// ===

Term wnf(device Term* heap, device Term* stack, device uint32_t* book, thread State& st, Term term) {
  uint32_t s_pos = 0;
  Term next = term;
  Term whnf = 0;
  uint8_t phase = 0;

  while (true) {
    if (phase == 0) {
      switch (tag(next)) {
        case VAR: {
          uint32_t loc = val(next);
          Term h = heap[loc];
          if (sub_of(h)) { next = clear_sub(h); continue; }
          whnf = next; phase = 1; continue;
        }
        case CO0: case CO1: {
          uint32_t loc = val(next);
          Term h = heap[loc];
          if (sub_of(h)) { next = clear_sub(h); continue; }
          stack[s_pos++] = next; next = h; continue;
        }
        case APP: { stack[s_pos++] = next; next = heap[val(next)]; continue; }
        case DUP: { next = heap[val(next) + 1]; continue; }
        case REF: {
          uint32_t book_loc = book[ext(next)];
          if (book_loc != 0) { next = make_alo(heap, st, 0, book_loc); continue; }
          whnf = next; phase = 1; continue;
        }
        case ALO: {
          uint64_t pair = heap[val(next)];
          uint32_t tm_loc = uint32_t(pair & VAL_MASK);
          uint32_t ls_loc = uint32_t(pair >> 32);
          Term bk = heap[tm_loc];
          uint8_t bt = tag(bk); uint32_t bv = val(bk); uint32_t be = ext(bk);
          switch (bt) {
            case VAR: next = alo_var(heap, ls_loc, bv); continue;
            case CO0: next = alo_cop(heap, ls_loc, bv, be, 0); continue;
            case CO1: next = alo_cop(heap, ls_loc, bv, be, 1); continue;
            case LAM: next = alo_lam(heap, st, ls_loc, bv); continue;
            case APP: next = alo_node(heap, st, ls_loc, bv, APP, be, 2); continue;
            case SUP: next = alo_node(heap, st, ls_loc, bv, SUP, be, 2); continue;
            case MAT: next = alo_node(heap, st, ls_loc, bv, MAT, be, 2); continue;
            case DRY: next = alo_node(heap, st, ls_loc, bv, DRY, be, 2); continue;
            case DUP: next = alo_dup(heap, st, ls_loc, bv, be); continue;
            case REF: case NAM: case ERA: next = bk; continue;
            default:
              if (bt >= CTR && bt <= CTR + CTR_MAX_ARI) { next = alo_node(heap, st, ls_loc, bv, bt, be, bt - CTR); continue; }
              whnf = next; phase = 1; continue;
          }
        }
        default: whnf = next; phase = 1; continue;
      }
    }
    if (s_pos == 0) return whnf;
    Term frame = stack[--s_pos];
    uint8_t ft = tag(frame); uint8_t wt = tag(whnf);
    if (ft == APP) {
      Term arg = heap[val(frame) + 1];
      switch (wt) {
        case ERA: whnf = app_era(st); continue;
        case NAM: case DRY: whnf = app_stuck(heap, st, whnf, arg); continue;
        case LAM: next = app_lam(heap, st, whnf, arg); phase = 0; continue;
        case SUP: next = app_sup(heap, st, frame, whnf); phase = 0; continue;
        case MAT: stack[s_pos++] = whnf; next = arg; phase = 0; continue;
        default: whnf = App(heap, st, whnf, arg); continue;
      }
    } else if (ft == MAT) {
      switch (wt) {
        case ERA: whnf = app_era(st); continue;
        case SUP: next = app_mat_sup(heap, st, frame, whnf); phase = 0; continue;
        default:
          if (wt >= CTR && wt <= CTR + CTR_MAX_ARI) { next = app_mat_ctr(heap, st, frame, whnf); phase = 0; continue; }
          whnf = App(heap, st, frame, whnf); continue;
      }
    } else if (ft == CO0 || ft == CO1) {
      uint8_t side = (ft == CO0) ? 0 : 1;
      uint32_t loc = val(frame); uint32_t lab = ext(frame);
      switch (wt) {
        case LAM: next = dup_lam(heap, st, lab, loc, side, whnf); phase = 0; continue;
        case SUP: next = dup_sup(heap, st, lab, loc, side, whnf); phase = 0; continue;
        case ERA: case NAM: whnf = dup_node(heap, st, lab, loc, side, whnf); continue;
        case MAT: case DRY: next = dup_node(heap, st, lab, loc, side, whnf); phase = 0; continue;
        default:
          if (wt >= CTR && wt <= CTR + CTR_MAX_ARI) { next = dup_node(heap, st, lab, loc, side, whnf); phase = 0; continue; }
          uint32_t new_loc = heap_alloc(st, 1);
          heap[new_loc] = whnf;
          subst_var(heap, loc, new_term(0, side == 0 ? CO1 : CO0, lab, new_loc));
          whnf = new_term(0, side == 0 ? CO0 : CO1, lab, new_loc);
          continue;
      }
    }
  }
}

// SNF
// ===

struct SNFFrame {
  Term     term;
  uint32_t write_loc;
  uint32_t depth;
  uint8_t  phase;
};

constant uint32_t SNF_ROOT = 0xFFFFFFFF;

Term snf(device Term* heap, device Term* stack, device uint32_t* book, thread State& st, Term term) {
  device SNFFrame* snf_stack = (device SNFFrame*)(stack + 1024*1024);
  uint32_t snf_pos = 0;

  snf_stack[snf_pos++] = SNFFrame{term, SNF_ROOT, 0, 0};

  Term result = 0;

  while (snf_pos > 0) {
    SNFFrame frame = snf_stack[--snf_pos];

    if (frame.phase == 0) {
      Term t = wnf(heap, stack, book, st, frame.term);
      uint32_t ari = arity_of(t);

      if (ari == 0) {
        if (frame.write_loc != SNF_ROOT) {
          heap[frame.write_loc] = t;
        } else {
          result = t;
        }
      } else {
        uint32_t loc = val(t);
        snf_stack[snf_pos++] = SNFFrame{t, frame.write_loc, frame.depth, 1};

        if (tag(t) == LAM) {
          Term body = heap[loc];
          subst_var(heap, loc, Nam(frame.depth + 1));
          snf_stack[snf_pos++] = SNFFrame{body, loc, frame.depth + 1, 0};
        } else {
          for (int32_t i = int32_t(ari) - 1; i >= 0; i--) {
            snf_stack[snf_pos++] = SNFFrame{heap[loc + uint32_t(i)], loc + uint32_t(i), frame.depth, 0};
          }
        }
      }
    } else {
      if (frame.write_loc != SNF_ROOT) {
        heap[frame.write_loc] = frame.term;
      } else {
        result = frame.term;
      }
    }
  }

  return result;
}

// Kernel
// ======
//
// Single-threaded kernel for testing

kernel void hvm_run(
  device Term*     heap      [[buffer(0)]],
  device Term*     stack     [[buffer(1)]],
  device uint32_t* book      [[buffer(2)]],
  device uint32_t* results   [[buffer(3)]],
  constant uint32_t& init_alloc   [[buffer(4)]],
  constant uint32_t& main_ref     [[buffer(5)]],
  uint tid [[thread_position_in_grid]]
) {
  if (tid != 0) return;

  State st;
  st.alloc = init_alloc;
  st.itrs  = 0;

  // Create @main reference
  Term main_term = new_term(0, REF, main_ref, 0);

  // Run SNF
  Term result = snf(heap, stack, book, st, main_term);

  // Store results
  results[0] = st.alloc;
  results[1] = uint32_t(st.itrs & 0xFFFFFFFF);
  results[2] = uint32_t(st.itrs >> 32);
  results[3] = uint32_t(tag(result));
  results[4] = val(result);
}

// Multi-threaded Kernel
// =====================
//
// Memory layout mimics CUDA implementation:
//   heap: [book region | warp0 slice | warp1 slice | ...]
//   Within each warp slice: interleaved by lane for coalesced access
//   Thread N in warp W owns indices: N, N+32, N+64, ... within its warp's slice
//
// CUDA-style warp-coalesced memory layout for GPU efficiency.
// Physical address for thread tid accessing logical idx (where idx >= book_size):
//   warp_id = tid / simd_width
//   lane_id = tid % simd_width
//   local_idx = idx - book_size
//   addr = book_size + warp_id * warp_slice_size + local_idx * simd_width + lane_id
//
// This ensures that when threads in a warp access the same logical index,
// the physical addresses are consecutive and can be coalesced into a single
// memory transaction.
struct Heap {
  device Term*  base;
  uint32_t      book_size;
  uint64_t      warp_slice_size;  // = heap_per_thr * simd_width
  uint32_t      warp_id;          // tid / simd_width
  uint32_t      lane_id;          // tid % simd_width
  uint32_t      simd_width;       // SIMD group width (32 on Apple Silicon)

  // Read from heap with coalesced access
  inline Term get(uint32_t idx) const {
    if (idx < book_size) {
      return base[idx];
    } else {
      uint32_t local_idx = idx - book_size;
      return base[book_size + warp_id * warp_slice_size + uint64_t(local_idx) * simd_width + lane_id];
    }
  }

  // Write to heap with coalesced access
  inline void set(uint32_t idx, Term val) const {
    if (idx < book_size) {
      base[idx] = val;
    } else {
      uint32_t local_idx = idx - book_size;
      base[book_size + warp_id * warp_slice_size + uint64_t(local_idx) * simd_width + lane_id] = val;
    }
  }
};

// State for multi-threaded version
struct StateMT {
  uint32_t alloc;  // Logical allocation pointer (starts at book_size)
  uint64_t itrs;
};

inline uint32_t heap_alloc_mt(thread StateMT& st, uint32_t size) {
  uint32_t at = st.alloc;
  st.alloc += size;
  return at;
}

// Substitution helpers for MT
inline void subst_var_mt(const thread Heap& heap, uint32_t loc, Term v) {
  heap.set(loc, mark_sub(v));
}

inline Term subst_cop_mt(const thread Heap& heap, uint8_t side, uint32_t loc, Term r0, Term r1) {
  heap.set(loc, mark_sub(side == 0 ? r1 : r0));
  return side == 0 ? r0 : r1;
}

// Term constructors for MT
inline Term LamMT(const thread Heap& heap, thread StateMT& st, Term bod) {
  uint32_t loc = heap_alloc_mt(st, 1);
  heap.set(loc, bod);
  return new_term(0, LAM, 0, loc);
}

inline Term AppMT(const thread Heap& heap, thread StateMT& st, Term fun, Term arg) {
  uint32_t loc = heap_alloc_mt(st, 2);
  heap.set(loc + 0, fun);
  heap.set(loc + 1, arg);
  return new_term(0, APP, 0, loc);
}

inline Term SupMT(const thread Heap& heap, thread StateMT& st, uint32_t lab, Term tm0, Term tm1) {
  uint32_t loc = heap_alloc_mt(st, 2);
  heap.set(loc + 0, tm0);
  heap.set(loc + 1, tm1);
  return new_term(0, SUP, lab, loc);
}

inline Term DryMT(const thread Heap& heap, thread StateMT& st, Term tm0, Term tm1) {
  uint32_t loc = heap_alloc_mt(st, 2);
  heap.set(loc + 0, tm0);
  heap.set(loc + 1, tm1);
  return new_term(0, DRY, 0, loc);
}

inline Term DupMT(const thread Heap& heap, thread StateMT& st, uint32_t lab, Term v, Term bod) {
  uint32_t loc = heap_alloc_mt(st, 2);
  heap.set(loc + 0, v);
  heap.set(loc + 1, bod);
  return new_term(0, DUP, lab, loc);
}

// Clone helpers for MT
inline Copy clone_mt(const thread Heap& heap, thread StateMT& st, uint32_t lab, Term v) {
  uint32_t loc = heap_alloc_mt(st, 1);
  heap.set(loc, v);
  return clone_at(loc, lab);
}

// Beta interactions for MT
inline Term app_era_mt(thread StateMT& st) {
  st.itrs++;
  return Era();
}

inline Term app_stuck_mt(const thread Heap& heap, thread StateMT& st, Term fun, Term arg) {
  st.itrs++;
  return DryMT(heap, st, fun, arg);
}

inline Term app_lam_mt(const thread Heap& heap, thread StateMT& st, Term lam, Term arg) {
  st.itrs++;
  uint32_t loc  = val(lam);
  Term     body = heap.get(loc);
  subst_var_mt(heap, loc, arg);
  return body;
}

inline Term app_sup_mt(const thread Heap& heap, thread StateMT& st, Term app, Term sup) {
  st.itrs++;
  uint32_t app_loc = val(app);
  uint32_t sup_loc = val(sup);
  uint32_t lab     = ext(sup);
  Term     arg     = heap.get(app_loc + 1);
  Term     tm1     = heap.get(sup_loc + 1);
  uint32_t loc     = heap_alloc_mt(st, 3);
  heap.set(loc + 2, arg);
  Copy D = clone_at(loc + 2, lab);
  heap.set(sup_loc + 1, D.k0);
  Term ap0 = new_term(0, APP, 0, sup_loc);
  uint32_t ap1_loc = loc;
  heap.set(ap1_loc + 0, tm1);
  heap.set(ap1_loc + 1, D.k1);
  Term ap1 = new_term(0, APP, 0, ap1_loc);
  heap.set(app_loc + 0, ap0);
  heap.set(app_loc + 1, ap1);
  return new_term(0, SUP, lab, app_loc);
}

// Match interactions for MT
inline Term app_mat_sup_mt(const thread Heap& heap, thread StateMT& st, Term mat, Term sup) {
  st.itrs++;
  uint32_t lab = ext(sup);
  Copy M       = clone_mt(heap, st, lab, mat);
  uint32_t loc = val(sup);
  Term a       = heap.get(loc + 0);
  Term b       = heap.get(loc + 1);
  return SupMT(heap, st, lab, AppMT(heap, st, M.k0, a), AppMT(heap, st, M.k1, b));
}

inline Term app_mat_ctr_mt(const thread Heap& heap, thread StateMT& st, Term mat, Term ctr) {
  st.itrs++;
  uint32_t ari = uint32_t(tag(ctr) - CTR);
  if (ext(mat) == ext(ctr)) {
    Term res = heap.get(val(mat));
    for (uint32_t i = 0; i < ari; i++) {
      res = AppMT(heap, st, res, heap.get(val(ctr) + i));
    }
    return res;
  } else {
    return AppMT(heap, st, heap.get(val(mat) + 1), ctr);
  }
}

// Dup interactions for MT
inline Term dup_lam_mt(const thread Heap& heap, thread StateMT& st, uint32_t lab, uint32_t loc, uint8_t side, Term lam) {
  st.itrs++;
  uint32_t lam_loc = val(lam);
  Term     bod     = heap.get(lam_loc);
  uint32_t a       = heap_alloc_mt(st, 5);
  heap.set(a + 4, bod);
  Copy B  = clone_at(a + 4, lab);
  heap.set(a + 2, Var(a));
  heap.set(a + 3, Var(a + 1));
  Term su = new_term(0, SUP, lab, a + 2);
  heap.set(a + 0, B.k0);
  heap.set(a + 1, B.k1);
  Term l0 = new_term(0, LAM, 0, a + 0);
  Term l1 = new_term(0, LAM, 0, a + 1);
  subst_var_mt(heap, lam_loc, su);
  return subst_cop_mt(heap, side, loc, l0, l1);
}

inline Term dup_sup_mt(const thread Heap& heap, thread StateMT& st, uint32_t lab, uint32_t loc, uint8_t side, Term sup) {
  st.itrs++;
  uint32_t sup_loc = val(sup);
  uint32_t sup_lab = ext(sup);
  if (lab == sup_lab) {
    Term tm0 = heap.get(sup_loc + 0);
    Term tm1 = heap.get(sup_loc + 1);
    return subst_cop_mt(heap, side, loc, tm0, tm1);
  } else {
    Copy A      = clone_at(sup_loc + 0, lab);
    Copy B      = clone_at(sup_loc + 1, lab);
    uint32_t a  = heap_alloc_mt(st, 4);
    heap.set(a + 0, A.k0);
    heap.set(a + 1, B.k0);
    heap.set(a + 2, A.k1);
    heap.set(a + 3, B.k1);
    Term s0     = new_term(0, SUP, sup_lab, a + 0);
    Term s1     = new_term(0, SUP, sup_lab, a + 2);
    return subst_cop_mt(heap, side, loc, s0, s1);
  }
}

inline Term dup_node_mt(const thread Heap& heap, thread StateMT& st, uint32_t lab, uint32_t loc, uint8_t side, Term term) {
  st.itrs++;
  uint32_t ari = arity_of(term);
  if (ari == 0) {
    subst_var_mt(heap, loc, term);
    return term;
  }
  uint32_t t_loc = val(term);
  uint32_t t_ext = ext(term);
  uint8_t  t_tag = tag(term);

  if (ari == 1) {
    Copy A = clone_mt(heap, st, lab, heap.get(t_loc));
    uint32_t loc0 = heap_alloc_mt(st, 1);
    uint32_t loc1 = heap_alloc_mt(st, 1);
    heap.set(loc0, A.k0);
    heap.set(loc1, A.k1);
    Term r0 = new_term(0, t_tag, t_ext, loc0);
    Term r1 = new_term(0, t_tag, t_ext, loc1);
    return subst_cop_mt(heap, side, loc, r0, r1);
  }

  if (ari == 2) {
    Copy A = clone_mt(heap, st, lab, heap.get(t_loc + 0));
    Copy B = clone_mt(heap, st, lab, heap.get(t_loc + 1));
    uint32_t loc0 = heap_alloc_mt(st, 2);
    uint32_t loc1 = heap_alloc_mt(st, 2);
    heap.set(loc0 + 0, A.k0);
    heap.set(loc0 + 1, B.k0);
    heap.set(loc1 + 0, A.k1);
    heap.set(loc1 + 1, B.k1);
    Term r0 = new_term(0, t_tag, t_ext, loc0);
    Term r1 = new_term(0, t_tag, t_ext, loc1);
    return subst_cop_mt(heap, side, loc, r0, r1);
  }

  Copy copies[16];
  for (uint32_t i = 0; i < ari; i++) {
    copies[i] = clone_mt(heap, st, lab, heap.get(t_loc + i));
  }
  uint32_t loc0 = heap_alloc_mt(st, ari);
  uint32_t loc1 = heap_alloc_mt(st, ari);
  for (uint32_t i = 0; i < ari; i++) {
    heap.set(loc0 + i, copies[i].k0);
    heap.set(loc1 + i, copies[i].k1);
  }
  Term r0 = new_term(0, t_tag, t_ext, loc0);
  Term r1 = new_term(0, t_tag, t_ext, loc1);
  return subst_cop_mt(heap, side, loc, r0, r1);
}

// Alloc helpers for MT
inline uint32_t bind_at_mt(const thread Heap& heap, uint32_t ls, uint32_t idx) {
  for (uint32_t i = 0; i < idx && ls != 0; i++) {
    ls = uint32_t(heap.get(ls) & VAL_MASK);
  }
  return (ls != 0) ? uint32_t(heap.get(ls) >> 32) : 0;
}

inline uint32_t make_bind_mt(const thread Heap& heap, thread StateMT& st, uint32_t tail, uint32_t loc) {
  uint32_t entry = heap_alloc_mt(st, 1);
  heap.set(entry, (uint64_t(loc) << 32) | uint64_t(tail));
  return entry;
}

inline Term make_alo_mt(const thread Heap& heap, thread StateMT& st, uint32_t ls_loc, uint32_t tm_loc) {
  uint32_t loc = heap_alloc_mt(st, 1);
  heap.set(loc, (uint64_t(ls_loc) << 32) | uint64_t(tm_loc));
  return new_term(0, ALO, 0, loc);
}

// Alloc interactions for MT
inline Term alo_var_mt(const thread Heap& heap, uint32_t ls_loc, uint32_t idx) {
  uint32_t bind = bind_at_mt(heap, ls_loc, idx);
  return bind != 0 ? Var(bind) : new_term(0, VAR, 0, idx);
}

inline Term alo_cop_mt(const thread Heap& heap, uint32_t ls_loc, uint32_t idx, uint32_t lab, uint8_t side) {
  uint32_t bind = bind_at_mt(heap, ls_loc, idx);
  uint8_t  tg   = side == 0 ? CO0 : CO1;
  return bind != 0 ? new_term(0, tg, lab, bind) : new_term(0, tg, lab, idx);
}

inline Term alo_lam_mt(const thread Heap& heap, thread StateMT& st, uint32_t ls_loc, uint32_t book_body_loc) {
  uint32_t lam_body = heap_alloc_mt(st, 1);
  uint32_t new_bind = make_bind_mt(heap, st, ls_loc, lam_body);
  heap.set(lam_body, make_alo_mt(heap, st, new_bind, book_body_loc));
  return new_term(0, LAM, 0, lam_body);
}

inline Term alo_dup_mt(const thread Heap& heap, thread StateMT& st, uint32_t ls_loc, uint32_t book_loc, uint32_t lab) {
  uint32_t dup_val  = heap_alloc_mt(st, 1);
  uint32_t new_bind = make_bind_mt(heap, st, ls_loc, dup_val);
  heap.set(dup_val, make_alo_mt(heap, st, ls_loc, book_loc));
  return DupMT(heap, st, lab, make_alo_mt(heap, st, ls_loc, book_loc), make_alo_mt(heap, st, new_bind, book_loc + 1));
}

inline Term alo_node_mt(const thread Heap& heap, thread StateMT& st, uint32_t ls_loc, uint32_t loc, uint8_t tg, uint32_t ex, uint32_t ari) {
  uint32_t new_loc = heap_alloc_mt(st, ari);
  for (uint32_t i = 0; i < ari; i++) {
    heap.set(new_loc + i, make_alo_mt(heap, st, ls_loc, loc + i));
  }
  return new_term(0, tg, ex, new_loc);
}

// WNF for multi-threaded
Term wnf_mt(const thread Heap& heap, device Term* stack, device uint32_t* book, thread StateMT& st, Term term) {
  uint32_t s_pos = 0;
  Term next = term;
  Term whnf = 0;
  uint8_t phase = 0;

  while (true) {
    if (phase == 0) {
      switch (tag(next)) {
        case VAR: {
          uint32_t loc = val(next);
          Term h = heap.get(loc);
          if (sub_of(h)) { next = clear_sub(h); continue; }
          whnf = next; phase = 1; continue;
        }
        case CO0: case CO1: {
          uint32_t loc = val(next);
          Term h = heap.get(loc);
          if (sub_of(h)) { next = clear_sub(h); continue; }
          stack[s_pos++] = next; next = h; continue;
        }
        case APP: { stack[s_pos++] = next; next = heap.get(val(next)); continue; }
        case DUP: { next = heap.get(val(next) + 1); continue; }
        case REF: {
          uint32_t book_loc = book[ext(next)];
          if (book_loc != 0) { next = make_alo_mt(heap, st, 0, book_loc); continue; }
          whnf = next; phase = 1; continue;
        }
        case ALO: {
          uint64_t pair = heap.get(val(next));
          uint32_t tm_loc = uint32_t(pair & VAL_MASK);
          uint32_t ls_loc = uint32_t(pair >> 32);
          // Book terms are always at ls_loc == 0 and read from book region (idx < book_size)
          Term bk = heap.get(tm_loc);
          uint8_t bt = tag(bk); uint32_t bv = val(bk); uint32_t be = ext(bk);
          switch (bt) {
            case VAR: next = alo_var_mt(heap, ls_loc, bv); continue;
            case CO0: next = alo_cop_mt(heap, ls_loc, bv, be, 0); continue;
            case CO1: next = alo_cop_mt(heap, ls_loc, bv, be, 1); continue;
            case LAM: next = alo_lam_mt(heap, st, ls_loc, bv); continue;
            case APP: next = alo_node_mt(heap, st, ls_loc, bv, APP, be, 2); continue;
            case SUP: next = alo_node_mt(heap, st, ls_loc, bv, SUP, be, 2); continue;
            case MAT: next = alo_node_mt(heap, st, ls_loc, bv, MAT, be, 2); continue;
            case DRY: next = alo_node_mt(heap, st, ls_loc, bv, DRY, be, 2); continue;
            case DUP: next = alo_dup_mt(heap, st, ls_loc, bv, be); continue;
            case REF: case NAM: case ERA: next = bk; continue;
            default:
              if (bt >= CTR && bt <= CTR + CTR_MAX_ARI) { next = alo_node_mt(heap, st, ls_loc, bv, bt, be, bt - CTR); continue; }
              whnf = next; phase = 1; continue;
          }
        }
        default: whnf = next; phase = 1; continue;
      }
    }
    if (s_pos == 0) return whnf;
    Term frame = stack[--s_pos];
    uint8_t ft = tag(frame); uint8_t wt = tag(whnf);
    if (ft == APP) {
      Term arg = heap.get(val(frame) + 1);
      switch (wt) {
        case ERA: whnf = app_era_mt(st); continue;
        case NAM: case DRY: whnf = app_stuck_mt(heap, st, whnf, arg); continue;
        case LAM: next = app_lam_mt(heap, st, whnf, arg); phase = 0; continue;
        case SUP: next = app_sup_mt(heap, st, frame, whnf); phase = 0; continue;
        case MAT: stack[s_pos++] = whnf; next = arg; phase = 0; continue;
        default: whnf = AppMT(heap, st, whnf, arg); continue;
      }
    } else if (ft == MAT) {
      switch (wt) {
        case ERA: whnf = app_era_mt(st); continue;
        case SUP: next = app_mat_sup_mt(heap, st, frame, whnf); phase = 0; continue;
        default:
          if (wt >= CTR && wt <= CTR + CTR_MAX_ARI) { next = app_mat_ctr_mt(heap, st, frame, whnf); phase = 0; continue; }
          whnf = AppMT(heap, st, frame, whnf); continue;
      }
    } else if (ft == CO0 || ft == CO1) {
      uint8_t side = (ft == CO0) ? 0 : 1;
      uint32_t loc = val(frame); uint32_t lab = ext(frame);
      switch (wt) {
        case LAM: next = dup_lam_mt(heap, st, lab, loc, side, whnf); phase = 0; continue;
        case SUP: next = dup_sup_mt(heap, st, lab, loc, side, whnf); phase = 0; continue;
        case ERA: case NAM: whnf = dup_node_mt(heap, st, lab, loc, side, whnf); continue;
        case MAT: case DRY: next = dup_node_mt(heap, st, lab, loc, side, whnf); phase = 0; continue;
        default:
          if (wt >= CTR && wt <= CTR + CTR_MAX_ARI) { next = dup_node_mt(heap, st, lab, loc, side, whnf); phase = 0; continue; }
          uint32_t new_loc = heap_alloc_mt(st, 1);
          heap.set(new_loc, whnf);
          subst_var_mt(heap, loc, new_term(0, side == 0 ? CO1 : CO0, lab, new_loc));
          whnf = new_term(0, side == 0 ? CO0 : CO1, lab, new_loc);
          continue;
      }
    }
  }
}

// SNF for multi-threaded
Term snf_mt(const thread Heap& heap, device Term* stack, device uint32_t* book, thread StateMT& st, Term term, uint32_t stack_size) {
  device SNFFrame* snf_stack = (device SNFFrame*)(stack + stack_size / 2);
  uint32_t snf_pos = 0;
  snf_stack[snf_pos++] = SNFFrame{term, SNF_ROOT, 0, 0};
  Term result = 0;

  while (snf_pos > 0) {
    SNFFrame frame = snf_stack[--snf_pos];
    if (frame.phase == 0) {
      Term t = wnf_mt(heap, stack, book, st, frame.term);
      uint32_t ari = arity_of(t);
      if (ari == 0) {
        if (frame.write_loc != SNF_ROOT) heap.set(frame.write_loc, t);
        else result = t;
      } else {
        uint32_t loc = val(t);
        snf_stack[snf_pos++] = SNFFrame{t, frame.write_loc, frame.depth, 1};
        if (tag(t) == LAM) {
          Term body = heap.get(loc);
          subst_var_mt(heap, loc, Nam(frame.depth + 1));
          snf_stack[snf_pos++] = SNFFrame{body, loc, frame.depth + 1, 0};
        } else {
          for (int32_t i = int32_t(ari) - 1; i >= 0; i--) {
            snf_stack[snf_pos++] = SNFFrame{heap.get(loc + uint32_t(i)), loc + uint32_t(i), frame.depth, 0};
          }
        }
      }
    } else {
      if (frame.write_loc != SNF_ROOT) heap.set(frame.write_loc, frame.term);
      else result = frame.term;
    }
  }
  return result;
}

// Multi-threaded kernel with CUDA-style warp-coalesced memory
kernel void hvm_run_mt(
  device Term*     global_heap   [[buffer(0)]],  // [book | warp0 slice | warp1 slice | ...]
  device Term*     global_stack  [[buffer(1)]],  // [thread0 | thread1 | ...]
  device uint32_t* book          [[buffer(2)]],  // Book dict (name -> loc)
  device uint64_t* itrs_out      [[buffer(3)]],  // Per-thread iteration counts
  device Term*     outputs       [[buffer(4)]],  // Per-thread output terms
  constant uint32_t& book_size        [[buffer(5)]],
  constant uint32_t& main_ref         [[buffer(6)]],
  constant uint32_t& num_threads      [[buffer(7)]],
  constant uint64_t& heap_per_thr     [[buffer(8)]],
  constant uint64_t& stack_per_thr    [[buffer(9)]],
  constant uint32_t& simd_width       [[buffer(10)]],  // Warp/SIMD size (32 on Apple Silicon)
  uint tid [[thread_position_in_grid]]
) {
  if (tid >= num_threads) return;

  // Compute warp/lane IDs for coalesced memory access
  uint32_t warp_id = tid / simd_width;
  uint32_t lane_id = tid % simd_width;
  uint64_t warp_slice_size = heap_per_thr * simd_width;

  // Create heap accessor with CUDA-style interleaved layout
  Heap heap;
  heap.base = global_heap;
  heap.book_size = book_size;
  heap.warp_slice_size = warp_slice_size;
  heap.warp_id = warp_id;
  heap.lane_id = lane_id;
  heap.simd_width = simd_width;

  // Stack: each thread gets stack_per_thr entries
  device Term* stack = global_stack + uint64_t(tid) * stack_per_thr;

  // Initialize state
  StateMT st;
  st.alloc = book_size;  // Logical allocation starts after book
  st.itrs  = 0;

  // Create @main reference
  Term main_term = new_term(0, REF, main_ref, 0);

  // Run SNF
  Term result = snf_mt(heap, stack, book, st, main_term, uint32_t(stack_per_thr));

  // Store results
  itrs_out[tid] = st.itrs;
  outputs[tid]  = result;
}
