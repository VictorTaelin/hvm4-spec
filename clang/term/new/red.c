fn Term term_new_red_at(u32 loc, Term lhs, Term rhs) {
  return term_new_at(loc, RED, 0, 2, (Term[]){lhs, rhs});
}

fn Term term_new_red(Term lhs, Term rhs) {
  return term_new_red_at(heap_alloc(2), lhs, rhs);
}
