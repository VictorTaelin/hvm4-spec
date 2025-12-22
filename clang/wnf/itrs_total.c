fn u64 wnf_itrs_total(void) {
  u64 sum = 0;
  u32 threads = thread_get_count();
  for (u32 i = 0; i < threads; i++) {
    sum += WNF_ITRS_BANKS[i].itrs;
  }
  return sum;
}
