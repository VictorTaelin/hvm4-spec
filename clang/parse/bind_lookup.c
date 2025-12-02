fn void parse_bind_lookup(u32 name, u32 depth, int *idx, u32 *lab, u32 *cloned) {
  for (int i = PARSE_BINDS_LEN - 1; i >= 0; i--) {
    if (PARSE_BINDS[i].name == name) {
      *idx = depth - 1 - PARSE_BINDS[i].depth;
      *lab = PARSE_BINDS[i].lab;
      *cloned = PARSE_BINDS[i].cloned;
      PARSE_BINDS[i].uses++;
      return;
    }
  }
  *idx = -1;
  *lab = 0;
  *cloned = 0;
}

fn u32 parse_bind_get_uses(void) {
  if (PARSE_BINDS_LEN > 0) {
    return PARSE_BINDS[PARSE_BINDS_LEN - 1].uses;
  }
  return 0;
}

fn u32 parse_bind_is_cloned(void) {
  if (PARSE_BINDS_LEN > 0) {
    return PARSE_BINDS[PARSE_BINDS_LEN - 1].cloned;
  }
  return 0;
}
