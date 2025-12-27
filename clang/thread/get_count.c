fn u32 thread_get_count(void) {
  return THREAD_COUNT == 0 ? 1 : THREAD_COUNT;
}
