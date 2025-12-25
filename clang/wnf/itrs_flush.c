fn void wnf_itrs_flush(u32 tid) {
  WNF_ITRS_BANKS[tid].itrs = WNF_ITRS_LOCAL;
}
