fn void heap_set(u32 loc, Term val) {
  __atomic_store_n(&HEAP[loc], val, __ATOMIC_RELAXED);
}
