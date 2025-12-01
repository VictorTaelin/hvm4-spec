fn Term term_unmark(Term t) {
  return t & ~(((u64)SUB_MASK) << SUB_SHIFT);
}
