// data/uset.c - open-addressing set for non-zero u32 keys.
//
// Context
// - Used by parallel normalization to track visited heap locations cheaply.
// - Optimized for insert/contains; there is no deletion.
//
// Design
// - Linear probing over a power-of-two table.
// - Key 0 is reserved as the empty slot sentinel.
// - Hash is multiplicative; index is masked by (cap - 1).
// - Grows by doubling when load exceeds ~70%.
//
// Notes
// - Not thread-safe; callers must synchronize externally.
// - cap_hint is rounded up to a power of two (minimum 2).
// - On allocation failure, the process exits with an error.
// - Because 0 is reserved, 0 is always reported as "not present".

// Uset stores the open-addressing table and its load metadata.
typedef struct {
  u32 *table;
  u32 cap;
  u32 mask;
  u32 len;
} Uset;

// Multiplicative hash for u32 keys (unmasked).
static inline u32 uset_hash(u32 key) {
  return key * 2654435761u;
}

// Round up to next power of two (minimum 2) for mask arithmetic.
static inline u32 uset_next_pow2(u32 x) {
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

// Initialize the set with a capacity >= cap_hint.
static inline void uset_init(Uset *set, u32 cap_hint) {
  u32 cap = uset_next_pow2(cap_hint);
  set->table = (u32 *)calloc(cap, sizeof(u32));
  if (!set->table) {
    fprintf(stderr, "uset: allocation failed\n");
    exit(1);
  }
  set->cap  = cap;
  set->mask = cap - 1;
  set->len  = 0;
}

// Release the backing table and reset the set state.
static inline void uset_free(Uset *set) {
  if (set->table) {
    free(set->table);
    set->table = NULL;
  }
  set->cap = 0;
  set->mask = 0;
  set->len = 0;
}

// Resize to new_cap and reinsert all keys.
static inline void uset_rehash(Uset *set, u32 new_cap) {
  u32 *old = set->table;
  u32 old_cap = set->cap;

  set->table = (u32 *)calloc(new_cap, sizeof(u32));
  if (!set->table) {
    fprintf(stderr, "uset: allocation failed\n");
    exit(1);
  }
  set->cap  = new_cap;
  set->mask = new_cap - 1;
  set->len  = 0;

  for (u32 i = 0; i < old_cap; i++) {
    u32 key = old[i];
    if (key == 0) {
      continue;
    }
    u32 idx = uset_hash(key) & set->mask;
    while (set->table[idx] != 0) {
      idx = (idx + 1) & set->mask;
    }
    set->table[idx] = key;
    set->len++;
  }

  free(old);
}

// Check whether key is present (0 is never present).
static inline u8 uset_has(Uset *set, u32 key) {
  if (key == 0) {
    return 0;
  }
  u32 idx = uset_hash(key) & set->mask;
  for (u32 i = 0; i < set->cap; i++) {
    u32 cur = set->table[idx];
    if (cur == 0) {
      return 0;
    }
    if (cur == key) {
      return 1;
    }
    idx = (idx + 1) & set->mask;
  }
  return 0;
}

// Insert key if missing; returns 1 if inserted, 0 if already present.
static inline u8 uset_add(Uset *set, u32 key) {
  if (key == 0) {
    return 0;
  }
  if ((set->len + 1) * 10 >= set->cap * 7) {
    u32 new_cap = set->cap << 1;
    if (new_cap < set->cap) {
      fprintf(stderr, "uset: allocation failed\n");
      exit(1);
    }
    uset_rehash(set, new_cap);
  }
  u32 idx = uset_hash(key) & set->mask;
  for (u32 i = 0; i < set->cap; i++) {
    u32 cur = set->table[idx];
    if (cur == key) {
      return 0;
    }
    if (cur == 0) {
      set->table[idx] = key;
      set->len++;
      return 1;
    }
    idx = (idx + 1) & set->mask;
  }
  uset_rehash(set, set->cap << 1);
  return uset_add(set, key);
}
