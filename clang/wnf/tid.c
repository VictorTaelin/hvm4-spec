static _Thread_local u32 WNF_TID = 0;

fn u32 wnf_tid(void) {
  return WNF_TID;
}

fn void wnf_set_tid(u32 tid) {
  WNF_TID = tid;
}
