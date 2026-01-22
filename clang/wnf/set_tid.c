fn void wnf_set_tid(u32 tid) {
  WNF_TID = tid;
  WNF_BANK = &WNF_BANKS[tid];
  WNF_ITRS_PTR = &WNF_ITRS_BANKS[tid].itrs;
  *WNF_ITRS_PTR = 0;
}
