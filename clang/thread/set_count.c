fn void thread_set_count(u32 n) {
  if (n == 0) {
    n = 1;
  }
  if (n > MAX_THREADS) {
    n = MAX_THREADS;
  }
  THREAD_COUNT = n;
}
