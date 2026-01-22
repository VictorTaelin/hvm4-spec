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
    u32 id = parse_name_ref(s);
    parse_consume(s, "=");
    PARSE_BINDS_LEN = 0;
    Term val        = parse_term(s, 0);
    u64  loc        = heap_alloc(1);
    HEAP[loc]       = val;
    BOOK[id]        = (u32)loc;
    parse_def(s);
    return;
  }
  parse_error(s, "definition or #include", parse_peek(s));
}
