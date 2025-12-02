fn void print_term_go(FILE *f, Term term, u32 depth);

fn void print_ctr(FILE *f, Term t, u32 d);

fn void print_mat_name(FILE *f, u32 nam) {
  if (nam == NAM_ZER) fputs("0n", f);
  else if (nam == NAM_SUC) fputs("1n+", f);
  else if (nam == NAM_NIL) fputs("[]", f);
  else if (nam == NAM_CON) fputs("<>", f);
  else { fputc('#', f); print_name(f, nam); }
}

// Prints APP and DRY chains as f(x,y,z)
fn void print_app(FILE *f, Term term, u32 depth) {
  Term spine[256];
  u32  len  = 0;
  Term curr = term;
  while ((term_tag(curr) == APP || term_tag(curr) == DRY) && len < 256) {
    u32 loc = term_val(curr);
    spine[len++] = HEAP[loc + 1];
    curr = HEAP[loc];
  }
  if (term_tag(curr) == LAM) {
    fputc('(', f);
    print_term_go(f, curr, depth);
    fputc(')', f);
  } else {
    print_term_go(f, curr, depth);
  }
  fputc('(', f);
  for (u32 i = 0; i < len; i++) {
    if (i > 0) {
      fputc(',', f);
    }
    print_term_go(f, spine[len - 1 - i], depth);
  }
  fputc(')', f);
}

fn void print_term_go(FILE *f, Term term, u32 depth) {
  switch (term_tag(term)) {
    case NAM: {
      // Print stuck variable as just the name
      print_name(f, term_ext(term));
      break;
    }
    case DRY: {
      // Print stuck application as f(x,y)
      print_app(f, term, depth);
      break;
    }
    case VAR: {
      print_name(f, term_val(term));
      break;
    }
    case NUM: {
      fprintf(f, "%u", term_val(term));
      break;
    }
    case REF: {
      fputc('@', f);
      print_name(f, term_ext(term));
      break;
    }
    case ERA: {
      fputs("&{}", f);
      break;
    }
    case CO0:
    case CO1: {
      print_name(f, term_val(term));
      fputs(term_tag(term) == CO0 ? "₀" : "₁", f);
      break;
    }
    case LAM: {
      u32 loc = term_val(term);
      u32 nam = depth + 1;
      fputs("λ", f);
      print_name(f, nam);
      fputc('.', f);
      print_term_go(f, HEAP[loc], depth + 1);
      break;
    }
    case APP: {
      print_app(f, term, depth);
      break;
    }
    case SUP: {
      u32 loc = term_val(term);
      fputc('&', f);
      print_name(f, term_ext(term));
      fputc('{', f);
      print_term_go(f, HEAP[loc + 0], depth);
      fputc(',', f);
      print_term_go(f, HEAP[loc + 1], depth);
      fputc('}', f);
      break;
    }
    case DUP: {
      u32 loc = term_val(term);
      u32 nam = depth + 1;
      fputc('!', f);
      print_name(f, nam);
      fputc('&', f);
      print_name(f, term_ext(term));
      fputc('=', f);
      print_term_go(f, HEAP[loc + 0], depth);
      fputc(';', f);
      print_term_go(f, HEAP[loc + 1], depth + 1);
      break;
    }
    case MAT: {
      u32 loc = term_val(term);
      fputs("λ{", f);
      print_mat_name(f, term_ext(term));
      fputc(':', f);
      print_term_go(f, HEAP[loc + 0], depth);
      fputc(';', f);
      print_term_go(f, HEAP[loc + 1], depth);
      fputc('}', f);
      break;
    }
    case SWI: {
      u32 loc = term_val(term);
      u32 num = term_ext(term);
      fputs("λ{", f);
      fprintf(f, "%u", num);
      fputc(':', f);
      print_term_go(f, HEAP[loc + 0], depth);
      fputc(';', f);
      print_term_go(f, HEAP[loc + 1], depth);
      fputc('}', f);
      break;
    }
    case USE: {
      u32 loc = term_val(term);
      fputs("λ{", f);
      print_term_go(f, HEAP[loc], depth);
      fputc('}', f);
      break;
    }
    case C00 ... C16: {
      print_ctr(f, term, depth);
      break;
    }
    case P00 ... P16: {
      u32 ari = term_tag(term) - P00;
      u32 loc = term_val(term);
      u32 nam = term_ext(term);
      fputs("@@", f);
      print_name(f, nam);
      fputc('(', f);
      for (u32 i = 0; i < ari; i++) {
        if (i > 0) {
          fputc(',', f);
        }
        print_term_go(f, HEAP[loc + i], depth);
      }
      fputc(')', f);
      break;
    }
    case ALO: {
      fputs("<ALO>", f);
      break;
    }
  }
}

fn void print_ctr(FILE *f, Term t, u32 d) {
  u32 nam = term_ext(t), loc = term_val(t), ari = term_tag(t) - C00;
  // Nat: count SUCs, print as Nn or Nn+x
  if (nam == NAM_ZER || nam == NAM_SUC) {
    u32 n = 0;
    while (term_tag(t) == C01 && term_ext(t) == NAM_SUC) { n++; t = HEAP[term_val(t)]; }
    fprintf(f, "%un", n);
    if (!(term_tag(t) == C00 && term_ext(t) == NAM_ZER)) { fputc('+', f); print_term_go(f, t, d); }
    return;
  }
  // Char: 'x'
  if (nam == NAM_CHR && ari == 1 && term_tag(HEAP[loc]) == NUM) {
    u32 c = term_val(HEAP[loc]);
    if (c >= 32 && c < 127) { fprintf(f, "'%c'", c); return; }
  }
  // List/String
  if (nam == NAM_NIL || nam == NAM_CON) {
    // Check if string (non-empty, all printable chars)
    int is_str = (nam == NAM_CON);
    for (Term x = t; term_tag(x) == C02 && term_ext(x) == NAM_CON; x = HEAP[term_val(x)+1]) {
      Term h = HEAP[term_val(x)];
      if (!(term_tag(h) == C01 && term_ext(h) == NAM_CHR && term_tag(HEAP[term_val(h)]) == NUM)) { is_str = 0; break; }
      u32 c = term_val(HEAP[term_val(h)]);
      if (c < 32 || c >= 127) { is_str = 0; break; }
    }
    Term end = t; while (term_tag(end) == C02 && term_ext(end) == NAM_CON) end = HEAP[term_val(end)+1];
    if (is_str && term_tag(end) == C00 && term_ext(end) == NAM_NIL) {
      fputc('"', f);
      for (Term x = t; term_tag(x) == C02; x = HEAP[term_val(x)+1])
        fputc(term_val(HEAP[term_val(HEAP[term_val(x)])]), f);
      fputc('"', f);
      return;
    }
    // Proper list: [a,b,c]
    if (term_tag(end) == C00 && term_ext(end) == NAM_NIL) {
      fputc('[', f);
      for (Term x = t; term_tag(x) == C02; x = HEAP[term_val(x)+1]) {
        if (x != t) fputc(',', f);
        print_term_go(f, HEAP[term_val(x)], d);
      }
      fputc(']', f);
      return;
    }
    // Improper list: h <> t
    if (nam == NAM_CON) {
      print_term_go(f, HEAP[loc], d); fputs(" <> ", f); print_term_go(f, HEAP[loc+1], d);
      return;
    }
  }
  // Default CTR
  fputc('#', f); print_name(f, nam); fputc('{', f);
  for (u32 i = 0; i < ari; i++) { if (i) fputc(',', f); print_term_go(f, HEAP[loc+i], d); }
  fputc('}', f);
}

fn void print_term(Term term) {
  print_term_go(stdout, term, 0);
}
