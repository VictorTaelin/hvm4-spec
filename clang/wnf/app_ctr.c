fn Term wnf_app_ctr(Term ctr, Term arg) {
  return term_new_ctr(_APP_, 2, (Term[]){ctr, arg});
}
