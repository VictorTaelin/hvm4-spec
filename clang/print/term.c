typedef struct {
  u32 loc;
  u32 name;
} LamBind;

typedef struct {
  u32 loc;
  u32 name;
  u32 lab;
} DupBind;

typedef struct {
  LamBind *lams;
  DupBind *dups;
  u32      lam_len;
  u32      lam_cap;
  u32      dup_len;
  u32      dup_cap;
  u32      dup_print;
  u32      next_lam;
  u32      next_dup;
  u8       lam_use_ext;
  u8       quoted;
  u32      subst;
} PrintState;

fn void print_term_go(FILE *f, Term term, u32 depth, PrintState *st);

fn void print_term_at(FILE *f, Term term, u32 depth, PrintState *st) {
  assert(!term_sub(term));
  print_term_go(f, term, depth, st);
}

fn void print_term_mode(FILE *f, Term term, u32 depth, u8 quoted, u32 subst, PrintState *st) {
  u8  old_quoted = st->quoted;
  u32 old_subst  = st->subst;
  st->quoted = quoted;
  st->subst  = subst;
  print_term_at(f, term, depth, st);
  st->quoted = old_quoted;
  st->subst  = old_subst;
}

fn void print_alpha_name(FILE *f, u32 n, char base) {
  if (n == 0) {
    fputc('_', f);
    return;
  }
  char buf[32];
  u32  len = 0;
  while (n > 0) {
    n--;
    buf[len++] = (char)(base + (n % 26));
    n /= 26;
  }
  for (u32 i = 0; i < len; i++) {
    fputc(buf[len - 1 - i], f);
  }
}

fn void print_lam_name(FILE *f, PrintState *st, u32 name) {
  if (st->lam_use_ext) {
    print_name(f, name);
  } else {
    print_alpha_name(f, name, 'a');
  }
}

fn void print_dup_name(FILE *f, u32 name) {
  print_alpha_name(f, name, 'A');
}

fn void print_state_init(PrintState *st, u8 lam_use_ext) {
  memset(st, 0, sizeof(*st));
  st->next_lam    = 1;
  st->next_dup    = 1;
  st->lam_use_ext = lam_use_ext;
}

fn void print_state_free(PrintState *st) {
  free(st->lams);
  free(st->dups);
}

fn void print_state_grow(void **buf, u32 *cap, u32 item_size) {
  u32 new_cap = *cap == 0 ? 16 : *cap * 2;
  void *next  = realloc(*buf, item_size * new_cap);
  if (next == NULL) {
    fprintf(stderr, "print_state: out of memory\n");
    exit(1);
  }
  *buf = next;
  *cap = new_cap;
}

fn u32 print_state_lam(PrintState *st, u32 loc, u32 hint) {
  for (u32 i = 0; i < st->lam_len; i++) {
    if (st->lams[i].loc == loc) {
      return st->lams[i].name;
    }
  }
  if (st->lam_len == st->lam_cap) {
    print_state_grow((void **)&st->lams, &st->lam_cap, sizeof(LamBind));
  }
  u32 name = 0;
  if (st->lam_use_ext && hint != 0) {
    name = hint;
  } else {
    name = st->next_lam++;
  }
  st->lams[st->lam_len] = (LamBind){.loc = loc, .name = name};
  st->lam_len++;
  return name;
}

fn u32 print_state_dup(PrintState *st, u32 loc, u32 lab) {
  for (u32 i = 0; i < st->dup_len; i++) {
    if (st->dups[i].loc == loc) {
      return st->dups[i].name;
    }
  }
  if (st->dup_len == st->dup_cap) {
    print_state_grow((void **)&st->dups, &st->dup_cap, sizeof(DupBind));
  }
  u32 name = st->next_dup++;
  st->dups[st->dup_len] = (DupBind){.loc = loc, .name = name, .lab = lab};
  st->dup_len++;
  return name;
}

fn u32 alo_subst_get(u32 ls_loc, u32 idx) {
  u32 ls = ls_loc;
  for (u32 i = 0; i < idx && ls != 0; i++) {
    ls = (u32)(HEAP[ls] & 0xFFFFFFFF);
  }
  return ls != 0 ? (u32)(HEAP[ls] >> 32) : 0;
}

fn void print_mat_name(FILE *f, u32 nam) {
  if (nam == NAM_ZER) {
    fputs("0n", f);
  } else if (nam == NAM_SUC) {
    fputs("1n+", f);
  } else if (nam == NAM_NIL) {
    fputs("[]", f);
  } else if (nam == NAM_CON) {
    fputs("<>", f);
  } else {
    fputc('#', f);
    print_name(f, nam);
  }
}

// Prints APP and DRY chains as f(x,y,z)
fn void print_app(FILE *f, Term term, u32 depth, PrintState *st) {
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
    print_term_at(f, curr, depth, st);
    fputc(')', f);
  } else {
    print_term_at(f, curr, depth, st);
  }
  fputc('(', f);
  for (u32 i = 0; i < len; i++) {
    if (i > 0) {
      fputc(',', f);
    }
    print_term_at(f, spine[len - 1 - i], depth, st);
  }
  fputc(')', f);
}

fn void print_ctr(FILE *f, Term t, u32 d, PrintState *st) {
  u32 nam = term_ext(t), loc = term_val(t), ari = term_tag(t) - C00;
  // Nat: count SUCs, print as Nn or Nn+x
  if (nam == NAM_ZER || nam == NAM_SUC) {
    u32 n = 0;
    while (term_tag(t) == C01 && term_ext(t) == NAM_SUC) {
      n++;
      t = HEAP[term_val(t)];
    }
    fprintf(f, "%un", n);
    if (!(term_tag(t) == C00 && term_ext(t) == NAM_ZER)) {
      fputc('+', f);
      print_term_at(f, t, d, st);
    }
    return;
  }
  // Char: 'x' or 'λ'
  if (nam == NAM_CHR && ari == 1 && term_tag(HEAP[loc]) == NUM) {
    u32 c = term_val(HEAP[loc]);
    if (c >= 32 && c != 127) {
      fputc('\'', f);
      print_utf8(f, c);
      fputc('\'', f);
      return;
    }
  }
  // List/String
  if (nam == NAM_NIL || nam == NAM_CON) {
    // Check if string (non-empty, all printable chars including Unicode)
    int is_str = (nam == NAM_CON);
    for (Term x = t; term_tag(x) == C02 && term_ext(x) == NAM_CON; x = HEAP[term_val(x) + 1]) {
      Term h = HEAP[term_val(x)];
      if (!(term_tag(h) == C01 && term_ext(h) == NAM_CHR)) {
        is_str = 0;
        break;
      }
      if (term_tag(HEAP[term_val(h)]) != NUM) {
        is_str = 0;
        break;
      }
      u32 c = term_val(HEAP[term_val(h)]);
      if (c < 32 || c == 127) {
        is_str = 0;
        break;
      }
    }
    Term end = t;
    while (term_tag(end) == C02 && term_ext(end) == NAM_CON) {
      end = HEAP[term_val(end) + 1];
    }
    if (is_str && term_tag(end) == C00 && term_ext(end) == NAM_NIL) {
      fputc('"', f);
      for (Term x = t; term_tag(x) == C02; x = HEAP[term_val(x) + 1]) {
        print_utf8(f, term_val(HEAP[term_val(HEAP[term_val(x)])]));
      }
      fputc('"', f);
      return;
    }
    // Proper list: [a,b,c]
    if (term_tag(end) == C00 && term_ext(end) == NAM_NIL) {
      fputc('[', f);
      for (Term x = t; term_tag(x) == C02; x = HEAP[term_val(x) + 1]) {
        if (x != t) {
          fputc(',', f);
        }
        print_term_at(f, HEAP[term_val(x)], d, st);
      }
      fputc(']', f);
      return;
    }
    // Improper list: h<>t
    if (nam == NAM_CON) {
      print_term_at(f, HEAP[loc], d, st);
      fputs("<>", f);
      print_term_at(f, HEAP[loc + 1], d, st);
      return;
    }
  }
  // Default CTR
  fputc('#', f);
  print_name(f, nam);
  fputc('{', f);
  for (u32 i = 0; i < ari; i++) {
    if (i) {
      fputc(',', f);
    }
    print_term_at(f, HEAP[loc + i], d, st);
  }
  fputc('}', f);
}

fn void print_term_go(FILE *f, Term term, u32 depth, PrintState *st) {
  u8  quoted = st->quoted;
  u32 subst  = st->subst;
  switch (term_tag(term)) {
    case NAM: {
      // Print stuck variable as just the name
      print_name(f, term_ext(term));
      break;
    }
    case DRY: {
      // Print stuck application as f(x,y)
      print_app(f, term, depth, st);
      break;
    }
    case VAR: {
      if (quoted) {
        u32 idx  = term_val(term);
        u32 bind = 0;
        if (idx >= depth) {
          bind = alo_subst_get(subst, idx - depth);
        }
        if (bind != 0) {
          Term val = HEAP[bind];
          if (term_sub(val)) {
            val = term_unmark(val);
            print_term_mode(f, val, depth, 0, 0, st);
          } else {
            print_term_mode(f, term_new_var(bind), depth, 0, 0, st);
          }
        } else {
          u32 nam = depth > idx ? depth - idx : 0;
          print_alpha_name(f, nam, 'a');
        }
      } else {
        u32 loc = term_val(term);
        if (loc != 0 && term_sub(HEAP[loc])) {
          print_term_mode(f, term_unmark(HEAP[loc]), depth, 0, 0, st);
        } else {
          u32 nam = print_state_lam(st, loc, 0);
          print_lam_name(f, st, nam);
        }
      }
      break;
    }
    case NUM: {
      fprintf(f, "%u", term_val(term));
      break;
    }
    case REF: {
      fputc('@', f);
      char *name = table_get(term_ext(term));
      if (name != NULL) {
        fputs(name, f);
      } else {
        print_name(f, term_ext(term));
      }
      break;
    }
    case ERA: {
      fputs("&{}", f);
      break;
    }
    case ANY: {
      fputc('*', f);
      break;
    }
    case CO0:
    case CO1: {
      if (quoted) {
        u32 idx  = term_val(term);
        u32 bind = 0;
        if (idx >= depth) {
          bind = alo_subst_get(subst, idx - depth);
        }
        if (bind != 0) {
          Term val = HEAP[bind];
          if (term_sub(val)) {
            val = term_unmark(val);
            print_term_mode(f, val, depth, 0, 0, st);
          } else {
            u8  tag = term_tag(term);
            u32 lab = term_ext(term);
            print_term_mode(f, term_new(0, tag, lab, bind), depth, 0, 0, st);
          }
        } else {
          u32 nam = depth > idx ? depth - idx : 0;
          if (nam == 0) {
            fputc('_', f);
          } else {
            print_alpha_name(f, nam, 'A');
          }
          fputs(term_tag(term) == CO0 ? "₀" : "₁", f);
        }
      } else {
        u32 loc = term_val(term);
        if (loc != 0 && term_sub(HEAP[loc])) {
          print_term_mode(f, term_unmark(HEAP[loc]), depth, 0, 0, st);
        } else {
          u32 nam = print_state_dup(st, loc, term_ext(term));
          print_dup_name(f, nam);
          fputs(term_tag(term) == CO0 ? "₀" : "₁", f);
        }
      }
      break;
    }
    case LAM: {
      u32 loc = term_val(term);
      fputs("λ", f);
      if (quoted) {
        print_alpha_name(f, depth + 1, 'a');
        fputc('.', f);
        print_term_at(f, HEAP[loc], depth + 1, st);
      } else {
        u32 hint = 0;
        if (st->lam_use_ext) {
          hint = term_ext(term) + 1;
        }
        u32 nam = print_state_lam(st, loc, hint);
        print_lam_name(f, st, nam);
        fputc('.', f);
        print_term_at(f, HEAP[loc], depth + 1, st);
      }
      break;
    }
    case APP: {
      print_app(f, term, depth, st);
      break;
    }
    case SUP: {
      u32 loc = term_val(term);
      fputc('&', f);
      print_name(f, term_ext(term));
      fputc('{', f);
      print_term_at(f, HEAP[loc + 0], depth, st);
      fputc(',', f);
      print_term_at(f, HEAP[loc + 1], depth, st);
      fputc('}', f);
      break;
    }
    case DUP: {
      u32 loc = term_val(term);
      if (quoted) {
        fputc('!', f);
        print_alpha_name(f, depth + 1, 'A');
        fputc('&', f);
        print_name(f, term_ext(term));
        fputc('=', f);
        print_term_at(f, HEAP[loc + 0], depth, st);
        fputc(';', f);
        print_term_at(f, HEAP[loc + 1], depth + 1, st);
      } else {
        print_state_dup(st, loc, term_ext(term));
        print_term_at(f, HEAP[loc + 1], depth, st);
      }
      break;
    }
    case MAT:
    case SWI: {
      fputs("λ{", f);
      Term cur = term;
      while (term_tag(cur) == MAT || term_tag(cur) == SWI) {
        u32 loc = term_val(cur);
        if (term_tag(cur) == SWI) {
          fprintf(f, "%u", term_ext(cur));
        } else {
          print_mat_name(f, term_ext(cur));
        }
        fputc(':', f);
        print_term_at(f, HEAP[loc + 0], depth, st);
        Term next = HEAP[loc + 1];
        if (term_tag(next) == MAT || term_tag(next) == SWI) {
          fputc(';', f);
        }
        cur = next;
      }
      // Handle tail: NUM(0) = empty, USE = wrapped default, other = default
      if (term_tag(cur) == NUM && term_val(cur) == 0) {
        // empty default - just close
      } else if (term_tag(cur) == USE) {
        fputc(';', f);
        print_term_at(f, HEAP[term_val(cur)], depth, st);
      } else {
        fputc(';', f);
        print_term_at(f, cur, depth, st);
      }
      fputc('}', f);
      break;
    }
    case USE: {
      u32 loc = term_val(term);
      fputs("λ{", f);
      print_term_at(f, HEAP[loc], depth, st);
      fputc('}', f);
      break;
    }
    case C00 ... C16: {
      print_ctr(f, term, depth, st);
      break;
    }
    case OP2: {
      u32 opr = term_ext(term);
      u32 loc = term_val(term);
      static const char *op_syms[] = {
        "+", "-", "*", "/", "%", "&&", "||", "^", "<<", ">>",
        "~", "==", "!=", "<", "<=", ">", ">="
      };
      fputc('(', f);
      print_term_at(f, HEAP[loc + 0], depth, st);
      fputc(' ', f);
      if (opr < 17) {
        fputs(op_syms[opr], f);
      } else {
        fprintf(f, "?%u", opr);
      }
      fputc(' ', f);
      print_term_at(f, HEAP[loc + 1], depth, st);
      fputc(')', f);
      break;
    }
    case DSU: {
      u32 loc = term_val(term);
      fputs("&(", f);
      print_term_at(f, HEAP[loc + 0], depth, st);
      fputs("){", f);
      print_term_at(f, HEAP[loc + 1], depth, st);
      fputc(',', f);
      print_term_at(f, HEAP[loc + 2], depth, st);
      fputc('}', f);
      break;
    }
    case DDU: {
      u32 loc = term_val(term);
      fputs("!(", f);
      print_term_at(f, HEAP[loc + 0], depth, st);
      fputs(")=", f);
      print_term_at(f, HEAP[loc + 1], depth, st);
      fputc(';', f);
      print_term_at(f, HEAP[loc + 2], depth, st);
      break;
    }
    case ALO: {
      u32 alo_loc = term_val(term);
      u64 pair    = HEAP[alo_loc];
      u32 tm_loc  = (u32)(pair & 0xFFFFFFFF);
      u32 ls_loc  = (u32)(pair >> 32);
      fputs("@{", f);
      print_term_mode(f, HEAP[tm_loc], 0, 1, ls_loc, st);
      fputc('}', f);
      break;
    }
    case RED: {
      u32 loc = term_val(term);
      print_term_at(f, HEAP[loc + 0], depth, st);
      fputs(" ~> ", f);
      print_term_at(f, HEAP[loc + 1], depth, st);
      break;
    }
    case EQL: {
      u32 loc = term_val(term);
      fputc('(', f);
      print_term_at(f, HEAP[loc + 0], depth, st);
      fputs(" === ", f);
      print_term_at(f, HEAP[loc + 1], depth, st);
      fputc(')', f);
      break;
    }
    case AND: {
      u32 loc = term_val(term);
      fputc('(', f);
      print_term_at(f, HEAP[loc + 0], depth, st);
      fputs(" .&. ", f);
      print_term_at(f, HEAP[loc + 1], depth, st);
      fputc(')', f);
      break;
    }
    case OR: {
      u32 loc = term_val(term);
      fputc('(', f);
      print_term_at(f, HEAP[loc + 0], depth, st);
      fputs(" .|. ", f);
      print_term_at(f, HEAP[loc + 1], depth, st);
      fputc(')', f);
      break;
    }
    case UNS: {
      u32 loc   = term_val(term);
      Term lamf = HEAP[loc];
      u32 locf  = term_val(lamf);
      Term lamv = HEAP[locf];
      u32 locv  = term_val(lamv);
      u32 hintf = st->lam_use_ext ? term_ext(lamf) + 1 : 0;
      u32 hintv = st->lam_use_ext ? term_ext(lamv) + 1 : 0;
      u32 namf  = print_state_lam(st, locf, hintf);
      u32 namv  = print_state_lam(st, locv, hintv);
      Term body = HEAP[locv];
      fputs("! ", f);
      print_lam_name(f, st, namf);
      fputs(" = λ ", f);
      print_lam_name(f, st, namv);
      fputs(" ; ", f);
      print_term_at(f, body, depth + 2, st);
      break;
    }
    case INC: {
      u32 loc = term_val(term);
      fputs("↑", f);
      print_term_at(f, HEAP[loc], depth, st);
      break;
    }
  }
}

fn void print_term_finish(FILE *f, PrintState *st) {
  while (st->dup_print < st->dup_len) {
    u32 idx = st->dup_print++;
    u32 loc = st->dups[idx].loc;
    u32 lab = st->dups[idx].lab;
    u32 nam = st->dups[idx].name;
    fputc('!', f);
    print_dup_name(f, nam);
    fputc('&', f);
    print_name(f, lab);
    fputc('=', f);
    Term val = HEAP[loc];
    if (term_sub(val)) {
      val = term_unmark(val);
    }
    print_term_at(f, val, 0, st);
    fputc(';', f);
  }
}

fn void print_term_ex(FILE *f, Term term, u8 lam_use_ext) {
  PrintState st;
  print_state_init(&st, lam_use_ext);
  print_term_at(f, term, 0, &st);
  print_term_finish(f, &st);
  print_state_free(&st);
}

fn void print_term(Term term) {
  print_term_ex(stdout, term, 0);
}

fn void print_term_quoted(Term term) {
  print_term_ex(stdout, term, 1);
}
