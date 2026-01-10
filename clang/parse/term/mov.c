fn Term parse_term(PState *s, u32 depth);

fn u32 mov_region_count_go(u64 loc, u32 lvl, u8 tgt, u32 ext, int *viol);
fn void mov_rebind_bjm_to_bjv_go(u64 loc, u32 lvl);
fn Term mov_rebind_bjm_to_bjv_term(Term t, u32 lvl);

fn u32 mov_region_count_term(Term t, u32 lvl, u8 tgt, u32 ext, int *viol) {
  if (*viol) {
    return 0;
  }
  u8  tg = term_tag(t);
  u32 vl = term_val(t);

  if (tg == tgt && vl == lvl && (tgt == BJV || term_ext(t) == ext)) {
    return 1;
  }

  u32 ari = term_arity(t);
  if (ari == 0) {
    return 0;
  }

  if (tg == LAM) {
    u32 cnt = mov_region_count_go(vl, lvl, tgt, ext, viol);
    if (cnt > 1) {
      *viol = 1;
    }
    return 0;
  }

  u32 sum = 0;
  for (u32 i = 0; i < ari; i++) {
    sum += mov_region_count_go(vl + i, lvl, tgt, ext, viol);
  }
  return sum;
}

fn u32 mov_region_count_go(u64 loc, u32 lvl, u8 tgt, u32 ext, int *viol) {
  return mov_region_count_term(HEAP[loc], lvl, tgt, ext, viol);
}

fn int mov_region_violates(Term body, u32 lvl, u8 tgt, u32 ext) {
  int viol = 0;
  u32 cnt = mov_region_count_term(body, lvl, tgt, ext, &viol);
  return viol || cnt > 1;
}

fn void mov_rebind_bjm_to_bjv_go(u64 loc, u32 lvl) {
  Term t = HEAP[loc];
  u8  tg = term_tag(t);
  u32 vl = term_val(t);
  u8  sub = term_sub_get(t);

  if (tg == BJM && vl == lvl) {
    Term v = term_new(0, BJV, 0, vl);
    HEAP[loc] = sub ? term_sub_set(v, 1) : v;
    return;
  }

  u32 ari = term_arity(t);
  if (ari == 0) {
    return;
  }

  for (u32 i = 0; i < ari; i++) {
    mov_rebind_bjm_to_bjv_go(vl + i, lvl);
  }
}

fn Term mov_rebind_bjm_to_bjv_term(Term t, u32 lvl) {
  u8  tg = term_tag(t);
  u32 vl = term_val(t);
  u8  sub = term_sub_get(t);

  if (tg == BJM && vl == lvl) {
    Term v = term_new(0, BJV, 0, vl);
    return sub ? term_sub_set(v, 1) : v;
  }

  u32 ari = term_arity(t);
  if (ari == 0) {
    return t;
  }

  for (u32 i = 0; i < ari; i++) {
    mov_rebind_bjm_to_bjv_go(vl + i, lvl);
  }
  return t;
}

fn Term parse_term_mov(PState *s, u32 depth) {
  parse_skip(s);
  u32 nam = parse_name(s);
  parse_skip(s);
  parse_consume(s, "=");
  Term val = parse_term(s, depth);
  parse_skip(s);
  parse_match(s, ";");
  parse_skip(s);
  parse_bind_push(nam, depth, 0, PBIND_MOV, 0);
  Term body = parse_term(s, depth + 1);
  u32 uses = parse_bind_get_uses();
  parse_bind_pop();
  u32 lvl = depth + 1;
  int viol = mov_region_violates(body, lvl, BJM, 0);
  if (viol) {
    body = mov_rebind_bjm_to_bjv_term(body, lvl);
    body = parse_auto_dup(body, lvl, lvl, BJV, 0);
    u64 lam_loc = heap_alloc(1);
    HEAP[lam_loc] = body;
    Term lam = term_new(0, LAM, lvl, lam_loc);
    return term_new_app(lam, val);
  }
  u64 loc = heap_alloc(2);
  HEAP[loc + 0] = val;
  HEAP[loc + 1] = body;
  if (uses > 2) {
    body = parse_auto_dup(body, lvl, lvl, BJM, 0);
    HEAP[loc + 1] = body;
  }
  return term_new(0, MOV, 0, loc);
}
