fn Term heap_take(u32 loc) {
  for (;;) {
    Term prev = __atomic_exchange_n(&HEAP[loc], 0, __ATOMIC_RELAXED);
    if (__builtin_expect(prev != 0, 1)) {
      return prev;
    }
    cpu_relax();
  }
}
