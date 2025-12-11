fn void parse_def(PState *s) {
  parse_skip(s);
  if (parse_at_end(s)) {
    return;
  }
  if (parse_match(s, "#include")) {
    parse_include(s);
    parse_def(s);
    return;
  }
  if (parse_match(s, "@")) {
    u32 nam = parse_name_ref(s);
    parse_consume(s, "=");
    PARSE_BINDS_LEN = 0;
    Term val        = parse_term(NONE, s, 0, 0);
    u64  loc        = heap_alloc(1);
    HEAP[loc]       = val;
    BOOK[nam]       = (u32)loc;
    parse_def(s);
    return;
  }
  parse_error(s, PERR_DEF_OR_INCLUDE());
}
