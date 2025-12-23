fn u64 wnf_itrs_thread(u32 tid) {
  u32 threads = thread_get_count();
  if (tid >= threads) {
    return 0;
  }
  return WNF_ITRS_BANKS[tid].itrs;
}
