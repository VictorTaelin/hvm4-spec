// ((f ~> &{}) a)
// -------------- app-red-era
// &{}
fn Term wnf_app_red_era(void) {
  ITRS++;
  return term_new_era();
}

// ((f ~> &L{x,y}) a)
// ------------------ app-red-sup
// ! F &L = f
// ! A &L = a
// &L{((F₀ ~> x) A₀)
//   ,((F₁ ~> y) A₁)}
fn Term wnf_app_red_sup(Term f, Term sup, Term arg) {
  ITRS++;
  u32  sup_loc = term_val(sup);
  u32  lab     = term_ext(sup);
  Term x       = HEAP[sup_loc + 0];
  Term y       = HEAP[sup_loc + 1];
  Copy F       = term_clone(lab, f);
  Copy A       = term_clone(lab, arg);
  Term r0      = term_new_app(term_new_red(F.k0, x), A.k0);
  Term r1      = term_new_app(term_new_red(F.k1, y), A.k1);
  return term_new_sup(lab, r0, r1);
}

// ((f ~> λx.g) a)
// --------------- app-red-lam
// x ← a
// (f x) ~> g
// Note: we substitute x with a, then return (f (Var x)) ~> g
// which effectively becomes (f a) ~> g[x:=a]
fn Term wnf_app_red_lam(Term f, Term lam, Term arg) {
  ITRS++;
  u32  lam_loc = term_val(lam);
  Term g       = HEAP[lam_loc];
  Term var_x   = term_new(0, VAR, 0, lam_loc);
  heap_subst_var(lam_loc, arg);
  return term_new_red(term_new_app(f, var_x), g);
}

// ((f ~> (g ~> h)) x)
// ------------------- app-red-red
// ((f x) ~> ((g ~> h) x))
fn Term wnf_app_red_red(Term f, Term red_inner, Term arg) {
  ITRS++;
  return term_new_red(term_new_app(f, arg), term_new_app(red_inner, arg));
}

// ((f ~> elim) (g ~> h))
// ---------------------- app-red-arg-red
// ((f (g ~> h)) ~> (elim (g ~> h)))
fn Term wnf_app_red_arg_red(Term f, Term elim, Term arg_red) {
  ITRS++;
  return term_new_red(term_new_app(f, arg_red), term_new_app(elim, arg_red));
}

// ((f ~> λ{#K:h; m}) &{})
// ----------------------- app-red-mat-era
// &{}
fn Term wnf_app_red_mat_era(void) {
  ITRS++;
  return term_new_era();
}

// ((f ~> λ{#K:h; m}) &L{a,b})
// --------------------------- app-red-mat-sup
// ! F &L = f
// ! H &L = h
// ! M &L = m
// &L{((F₀ ~> λ{#K:H₀; M₀}) a)
//   ,((F₁ ~> λ{#K:H₁; M₁}) b)}
fn Term wnf_app_red_mat_sup(Term f, Term mat, Term sup) {
  ITRS++;
  u32  mat_loc = term_val(mat);
  u32  mat_nam = term_ext(mat);
  u32  sup_loc = term_val(sup);
  u32  lab     = term_ext(sup);
  Term h       = HEAP[mat_loc + 0];
  Term m       = HEAP[mat_loc + 1];
  Term a       = HEAP[sup_loc + 0];
  Term b       = HEAP[sup_loc + 1];
  Copy F       = term_clone(lab, f);
  Copy H       = term_clone(lab, h);
  Copy M       = term_clone(lab, m);
  Term mat0    = term_new_mat(mat_nam, H.k0, M.k0);
  Term mat1    = term_new_mat(mat_nam, H.k1, M.k1);
  Term r0      = term_new_app(term_new_red(F.k0, mat0), a);
  Term r1      = term_new_app(term_new_red(F.k1, mat1), b);
  return term_new_sup(lab, r0, r1);
}

// ((f ~> λ{#K:h; m}) #K{a,b})
// --------------------------- app-red-mat-ctr-match
// ((λa.λb.(f #K{a,b}) ~> h) a b)
fn Term wnf_app_red_mat_ctr_match(Term f, Term mat, Term ctr) {
  ITRS++;
  u32  mat_loc = term_val(mat);
  u32  ctr_loc = term_val(ctr);
  u32  ctr_nam = term_ext(ctr);
  u32  ctr_ari = term_tag(ctr) - C00;
  Term h       = HEAP[mat_loc + 0];

  // Build: (λa.λb.(f #K{a,b}) ~> h) applied to ctr args
  u64 lam_locs[16];
  Term vars[16];
  for (u32 i = 0; i < ctr_ari; i++) {
    lam_locs[i] = heap_alloc(1);
    vars[i] = term_new(0, VAR, 0, lam_locs[i]);
  }
  Term inner = term_new_app(f, term_new_ctr(ctr_nam, ctr_ari, vars));

  // Wrap in lambdas from inside out
  Term body = inner;
  for (int32_t i = ctr_ari - 1; i >= 0; i--) {
    HEAP[lam_locs[i]] = body;
    body = term_new(0, LAM, 0, lam_locs[i]);
  }

  // Create red: body ~> h
  Term red_result = term_new_red(body, h);

  // Apply to original ctr args
  for (u32 i = 0; i < ctr_ari; i++) {
    red_result = term_new_app(red_result, HEAP[ctr_loc + i]);
  }

  return red_result;
}

// ((f ~> λ{#K:h; m}) #L{a,b})
// --------------------------- app-red-mat-ctr-miss
// ((f ~> m) #L{a,b})
fn Term wnf_app_red_mat_ctr_miss(Term f, Term mat, Term ctr) {
  ITRS++;
  u32  mat_loc = term_val(mat);
  Term m       = HEAP[mat_loc + 1];
  return term_new_app(term_new_red(f, m), ctr);
}

// ((f ~> λ{n:z;s}) &{})
// --------------------- app-red-swi-era
// &{}
fn Term wnf_app_red_swi_era(void) {
  ITRS++;
  return term_new_era();
}

// ((f ~> λ{n:z;s}) &L{a,b})
// ------------------------- app-red-swi-sup
// ! F &L = f
// ! Z &L = z
// ! S &L = s
// &L{((F₀ ~> λ{n:Z₀;S₀}) a)
//   ,((F₁ ~> λ{n:Z₁;S₁}) b)}
fn Term wnf_app_red_swi_sup(Term f, Term swi, Term sup) {
  ITRS++;
  u32  swi_loc = term_val(swi);
  u32  swi_num = term_ext(swi);
  u32  sup_loc = term_val(sup);
  u32  lab     = term_ext(sup);
  Term z       = HEAP[swi_loc + 0];
  Term s       = HEAP[swi_loc + 1];
  Term a       = HEAP[sup_loc + 0];
  Term b       = HEAP[sup_loc + 1];
  Copy F       = term_clone(lab, f);
  Copy Z       = term_clone(lab, z);
  Copy S       = term_clone(lab, s);
  Term swi0    = term_new_swi(swi_num, Z.k0, S.k0);
  Term swi1    = term_new_swi(swi_num, Z.k1, S.k1);
  Term r0      = term_new_app(term_new_red(F.k0, swi0), a);
  Term r1      = term_new_app(term_new_red(F.k1, swi1), b);
  return term_new_sup(lab, r0, r1);
}

// ((f ~> λ{n:z;s}) #n)
// -------------------- app-red-swi-match
// (f #n) ~> z
fn Term wnf_app_red_swi_match(Term f, Term swi, Term num) {
  ITRS++;
  u32  swi_loc = term_val(swi);
  Term z       = HEAP[swi_loc + 0];
  return term_new_red(term_new_app(f, num), z);
}

// ((f ~> λ{n:z;s}) #m) where m != n
// ---------------------------------- app-red-swi-miss
// ((λp.(f (1+p)) ~> s) #(m-1))
fn Term wnf_app_red_swi_miss(Term f, Term swi, Term num) {
  ITRS++;
  u32  swi_loc = term_val(swi);
  u32  num_val = term_val(num);
  Term s       = HEAP[swi_loc + 1];

  // Build λp.(f (1+p))
  u64 lam_loc  = heap_alloc(1);
  Term var_p   = term_new(0, VAR, 0, lam_loc);
  Term succ_p  = term_new_op2(OP_ADD, term_new_num(1), var_p);
  Term body    = term_new_app(f, succ_p);
  HEAP[lam_loc] = body;
  Term lam     = term_new(0, LAM, 0, lam_loc);

  // (lam ~> s) #(m-1)
  Term pred_num = term_new_num(num_val - 1);
  return term_new_app(term_new_red(lam, s), pred_num);
}

// ((f ~> λ{g}) &{})
// ----------------- app-red-use-era
// &{}
fn Term wnf_app_red_use_era(void) {
  ITRS++;
  return term_new_era();
}

// ((f ~> λ{g}) &L{a,b})
// --------------------- app-red-use-sup
// ! F &L = f
// ! G &L = g
// &L{((F₀ ~> λ{G₀}) a)
//   ,((F₁ ~> λ{G₁}) b)}
fn Term wnf_app_red_use_sup(Term f, Term use, Term sup) {
  ITRS++;
  u32  use_loc = term_val(use);
  u32  sup_loc = term_val(sup);
  u32  lab     = term_ext(sup);
  Term g       = HEAP[use_loc];
  Term a       = HEAP[sup_loc + 0];
  Term b       = HEAP[sup_loc + 1];
  Copy F       = term_clone(lab, f);
  Copy G       = term_clone(lab, g);
  Term use0    = term_new_use(G.k0);
  Term use1    = term_new_use(G.k1);
  Term r0      = term_new_app(term_new_red(F.k0, use0), a);
  Term r1      = term_new_app(term_new_red(F.k1, use1), b);
  return term_new_sup(lab, r0, r1);
}

// ((f ~> λ{g}) x)
// --------------- app-red-use-val
// (f x) ~> (g x)
fn Term wnf_app_red_use_val(Term f, Term use, Term val) {
  ITRS++;
  u32  use_loc = term_val(use);
  Term g       = HEAP[use_loc];
  return term_new_red(term_new_app(f, val), term_new_app(g, val));
}

// ((f ~> #K{...}) a)
// ------------------ app-red-ctr
// ^((f ~> #K{...}) a)
fn Term wnf_app_red_ctr(Term f, Term ctr, Term arg) {
  ITRS++;
  return term_new_dry(term_new_red(f, ctr), arg);
}

// ((f ~> ^n) a)
// ------------- app-red-nam
// ^((f ~> ^n) a)
fn Term wnf_app_red_nam(Term f, Term nam, Term arg) {
  ITRS++;
  return term_new_dry(term_new_red(f, nam), arg);
}

// ((f ~> ^(g x)) a)
// ----------------- app-red-dry
// ^((f ~> ^(g x)) a)
fn Term wnf_app_red_dry(Term f, Term dry, Term arg) {
  ITRS++;
  return term_new_dry(term_new_red(f, dry), arg);
}
