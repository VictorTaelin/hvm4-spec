// Quoted mode converts linked LAM/DUP binders into quoted LAM/DUP binders.
// When `quoted` is set, linked vars (VAR/DP0/DP1) become BJV/BJ0/BJ1 indices
// and the binders store their de Bruijn level in LAM.ext or BJ*. This is used
// by collapse to emit static terms.
#define SNF_SEEN_INIT (1u << 16)

typedef struct {
  u32 *table;
  u32 cap;
  u32 mask;
  u32 len;
} SnfSeen;

typedef struct {
  SnfSeen seen;
} SnfState;

static inline u32 snf_seen_hash(u32 loc) {
  return loc * 2654435761u;
}

static inline u32 snf_seen_next_pow2(u32 x) {
  if (x < 2u) {
    return 2u;
  }
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return x + 1;
}

static inline void snf_seen_init(SnfSeen *S, u32 cap_hint) {
  u32 cap = snf_seen_next_pow2(cap_hint);
  S->table = (u32 *)calloc(cap, sizeof(u32));
  if (!S->table) {
    fprintf(stderr, "snf: seen allocation failed\n");
    exit(1);
  }
  S->cap  = cap;
  S->mask = cap - 1;
  S->len  = 0;
}

static inline void snf_seen_free(SnfSeen *S) {
  if (S->table) {
    free(S->table);
    S->table = NULL;
  }
  S->cap = 0;
  S->mask = 0;
  S->len = 0;
}

static inline void snf_seen_rehash(SnfSeen *S, u32 new_cap) {
  u32 *old = S->table;
  u32 old_cap = S->cap;

  S->table = (u32 *)calloc(new_cap, sizeof(u32));
  if (!S->table) {
    fprintf(stderr, "snf: seen allocation failed\n");
    exit(1);
  }
  S->cap  = new_cap;
  S->mask = new_cap - 1;
  S->len  = 0;

  for (u32 i = 0; i < old_cap; i++) {
    u32 loc = old[i];
    if (loc == 0) {
      continue;
    }
    u32 idx = snf_seen_hash(loc) & S->mask;
    while (S->table[idx] != 0) {
      idx = (idx + 1) & S->mask;
    }
    S->table[idx] = loc;
    S->len++;
  }

  free(old);
}

static inline u8 snf_seen_has(SnfSeen *S, u32 loc) {
  if (loc == 0) {
    return 0;
  }
  u32 idx = snf_seen_hash(loc) & S->mask;
  for (u32 i = 0; i < S->cap; i++) {
    u32 cur = S->table[idx];
    if (cur == 0) {
      return 0;
    }
    if (cur == loc) {
      return 1;
    }
    idx = (idx + 1) & S->mask;
  }
  return 0;
}

static inline u8 snf_seen_add(SnfSeen *S, u32 loc) {
  if (loc == 0) {
    return 0;
  }
  if ((S->len + 1) * 10 >= S->cap * 7) {
    u32 new_cap = S->cap << 1;
    if (new_cap < S->cap) {
      fprintf(stderr, "snf: seen allocation failed\n");
      exit(1);
    }
    snf_seen_rehash(S, new_cap);
  }
  u32 idx = snf_seen_hash(loc) & S->mask;
  for (u32 i = 0; i < S->cap; i++) {
    u32 cur = S->table[idx];
    if (cur == loc) {
      return 0;
    }
    if (cur == 0) {
      S->table[idx] = loc;
      S->len++;
      return 1;
    }
    idx = (idx + 1) & S->mask;
  }
  snf_seen_rehash(S, S->cap << 1);
  return snf_seen_add(S, loc);
}

fn Term snf_at(u32 loc, u32 depth, u8 quoted, SnfState *st) {
  if (!snf_seen_add(&st->seen, loc)) {
    return heap_get(loc);
  }

  Term term = heap_get(loc);
  u8 tag = term_tag(term);
  if (!quoted && (tag == DP0 || tag == DP1)) {
    u32 dup_loc = term_val(term);
    if (dup_loc != 0 && !term_sub_get(heap_get_raw(dup_loc))) {
      if (snf_seen_has(&st->seen, dup_loc)) {
        return term;
      }
    }
  }

  term = wnf(term);
  heap_set(loc, term);
  tag = term_tag(term);

  if (!quoted && (tag == DP0 || tag == DP1)) {
    u32 dup_loc = term_val(term);
    if (dup_loc != 0 && !term_sub_get(heap_get_raw(dup_loc))) {
      if (!snf_seen_has(&st->seen, dup_loc)) {
        snf_at(dup_loc, 0, quoted, st);
      }
    }
    return term;
  }

  if (quoted && tag == DUP) {
    u32 dup_term_loc = term_val(term);
    u32 lab     = term_ext(term);
    Term val    = heap_get(dup_term_loc + 0);
    Term bod    = heap_get(dup_term_loc + 1);
    u32 level   = depth + 1;
    Term bj0    = term_new(0, BJ0, lab, level);
    Term bj1    = term_new(0, BJ1, lab, level);
    heap_set(dup_term_loc + 0, term_new_sup(lab, bj0, bj1));
    u64 val_loc = heap_alloc(1);
    heap_set(val_loc, val);
    snf_at((u32)val_loc, depth, quoted, st);
    snf_at(dup_term_loc + 1, depth + 1, quoted, st);
    u64 out_loc = heap_alloc(2);
    heap_set(out_loc + 0, heap_get(val_loc));
    heap_set(out_loc + 1, heap_get(dup_term_loc + 1));
    term = term_new(0, DUP, lab, out_loc);
    heap_set(loc, term);
    return term;
  }

  if (quoted) {
    if (tag == BJV || tag == BJ0 || tag == BJ1) {
      return term;
    }
    if (tag == VAR) {
      term = term_new(0, BJV, 0, depth);
      heap_set(loc, term);
      return term;
    }
    if (tag == DP0 || tag == DP1) {
      u8  bj_tag = tag == DP0 ? BJ0 : BJ1;
      u32 lab    = term_ext(term);
      term = term_new(0, bj_tag, lab, depth);
      heap_set(loc, term);
      return term;
    }
  }

  u32 ari = term_arity(term);
  if (ari == 0) {
    return term;
  }
  u32 tloc = term_val(term);
  if (tag == LAM) {
    if (quoted) {
      Term body  = heap_get(tloc);
      u32  level = depth + 1;
      heap_subst_var(tloc, term_new(0, BJV, 0, level));
      u64 tmp_loc = heap_alloc(1);
      heap_set(tmp_loc, body);
      snf_at((u32)tmp_loc, depth + 1, quoted, st);
      heap_set(tloc, heap_get(tmp_loc));
      term = term_new(0, LAM, level, tloc);
      heap_set(loc, term);
    } else {
      snf_at(tloc, depth + 1, quoted, st);
    }
  } else if (tag == DRY) {
    snf_at(tloc + 0, depth, quoted, st);
    snf_at(tloc + 1, depth, quoted, st);
  } else {
    for (u32 i = 0; i < ari; i++) {
      snf_at(tloc + i, depth, quoted, st);
    }
  }
  return term;
}
