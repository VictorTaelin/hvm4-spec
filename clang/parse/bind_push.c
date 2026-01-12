fn u32 parse_bind_push(u32 name, u32 depth, u32 lab, u32 cloned) {
  u32 bid = PARSE_BINDS_LEN++;
  PARSE_BINDS[bid] = (PBind){name, depth + 1, lab, cloned, 0, 0, 0};
  return bid;
}
