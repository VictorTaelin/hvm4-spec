fn Term heap_read(u32 loc) {
  Term term = __atomic_load_n(&HEAP[loc], __ATOMIC_RELAXED);
  if (__builtin_expect(term != 0, 1)) {
    return term;
  }
  do {
    cpu_relax();
    term = __atomic_load_n(&HEAP[loc], __ATOMIC_RELAXED);
  } while (term == 0);
  return term;
}
