fn Term term_mark(Term t) {
  return t | ((u64)1 << SUB_SHIFT);
}
