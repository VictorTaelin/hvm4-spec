// Forward declare wnf
fn Term wnf(Term term);

// @@add(a,b) - Addition of two NUMs
fn Term prim_add(Term a, Term b) {
  a = wnf(a);
  b = wnf(b);
  if (term_tag(a) != NUM || term_tag(b) != NUM) {
    fprintf(stderr, "@@add: expected NUMs\n");
    exit(1);
  }
  return term_new_num(term_val(a) + term_val(b));
}
