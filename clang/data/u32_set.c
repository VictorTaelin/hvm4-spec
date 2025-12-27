typedef struct {
  u32 *table;
  u32 cap;
  u32 mask;
  u32 len;
} U32Set;

static inline u32 u32_set_hash(u32 key) {
  return key * 2654435761u;
}

static inline u32 u32_set_next_pow2(u32 x) {
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

static inline void u32_set_init(U32Set *set, u32 cap_hint) {
  u32 cap = u32_set_next_pow2(cap_hint);
  set->table = (u32 *)calloc(cap, sizeof(u32));
  if (!set->table) {
    fprintf(stderr, "u32_set: allocation failed\n");
    exit(1);
  }
  set->cap  = cap;
  set->mask = cap - 1;
  set->len  = 0;
}

static inline void u32_set_free(U32Set *set) {
  if (set->table) {
    free(set->table);
    set->table = NULL;
  }
  set->cap = 0;
  set->mask = 0;
  set->len = 0;
}

static inline void u32_set_rehash(U32Set *set, u32 new_cap) {
  u32 *old = set->table;
  u32 old_cap = set->cap;

  set->table = (u32 *)calloc(new_cap, sizeof(u32));
  if (!set->table) {
    fprintf(stderr, "u32_set: allocation failed\n");
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
    u32 idx = u32_set_hash(key) & set->mask;
    while (set->table[idx] != 0) {
      idx = (idx + 1) & set->mask;
    }
    set->table[idx] = key;
    set->len++;
  }

  free(old);
}

static inline u8 u32_set_has(U32Set *set, u32 key) {
  if (key == 0) {
    return 0;
  }
  u32 idx = u32_set_hash(key) & set->mask;
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

static inline u8 u32_set_add(U32Set *set, u32 key) {
  if (key == 0) {
    return 0;
  }
  if ((set->len + 1) * 10 >= set->cap * 7) {
    u32 new_cap = set->cap << 1;
    if (new_cap < set->cap) {
      fprintf(stderr, "u32_set: allocation failed\n");
      exit(1);
    }
    u32_set_rehash(set, new_cap);
  }
  u32 idx = u32_set_hash(key) & set->mask;
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
  u32_set_rehash(set, set->cap << 1);
  return u32_set_add(set, key);
}
