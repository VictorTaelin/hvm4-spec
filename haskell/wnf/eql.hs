wnf_eql :: WnfEql
wnf_eql e s a b = case a of
  Era -> do
    inc_inters e
    wnf e s Era
  Sup l a0 a1 -> do
    inc_inters e
    k <- fresh e
    make_dup e k l b
    wnf_enter e s (Sup l (Eql a0 (Dp0 k)) (Eql a1 (Dp1 k)))
  _ -> do
    wnf_enter e (FEqlA a : s) b

wnf_eql_x :: WnfEql
wnf_eql_x e s a b = case b of
  Era -> do
    inc_inters e
    wnf e s Era
  Sup l b0 b1 -> do
    inc_inters e
    k <- fresh e
    make_dup e k l a
    wnf_enter e s (Sup l (Eql (Dp0 k) b0) (Eql (Dp1 k) b1))
  _ -> do
    wnf_eql_val e s a b

wnf_eql_val :: WnfEql
wnf_eql_val e s a b = case (a, b) of
  (Set, Set)       -> wnf_eql_set_set e s a b
  (All {}, All {}) -> wnf_eql_all_all e s a b
  (Lam {}, Lam {}) -> wnf_eql_lam_lam e s a b
  (Sig {}, Sig {}) -> wnf_eql_sig_sig e s a b
  (Tup {}, Tup {}) -> wnf_eql_tup_tup e s a b
  (Get {}, Get {}) -> wnf_eql_get_get e s a b
  (Emp, Emp)       -> wnf_eql_emp_emp e s a b
  (Efq, Efq)       -> wnf_eql_efq_efq e s a b
  (Uni, Uni)       -> wnf_eql_uni_uni e s a b
  (One, One)       -> wnf_eql_one_one e s a b
  (Use {}, Use {}) -> wnf_eql_use_use e s a b
  (Bol, Bol)       -> wnf_eql_bol_bol e s a b
  (Fal, Fal)       -> wnf_eql_fal_fal e s a b
  (Tru, Tru)       -> wnf_eql_tru_tru e s a b
  (Fal, Tru)       -> wnf_eql_fal_tru e s a b
  (Tru, Fal)       -> wnf_eql_tru_fal e s a b
  (If {}, If {})   -> wnf_eql_if_if   e s a b
  (Nat, Nat)       -> wnf_eql_nat_nat e s a b
  (Zer, Zer)       -> wnf_eql_zer_zer e s a b
  (Suc {}, Suc {}) -> wnf_eql_suc_suc e s a b
  (Swi {}, Swi {}) -> wnf_eql_swi_swi e s a b
  (Lst {}, Lst {}) -> wnf_eql_lst_lst e s a b
  (Nil, Nil)       -> wnf_eql_nil_nil e s a b
  (Con {}, Con {}) -> wnf_eql_con_con e s a b
  (Mat {}, Mat {}) -> wnf_eql_mat_mat e s a b
  (Nam {}, Nam {}) -> wnf_eql_nam_nam e s a b
  (Dry {}, Dry {}) -> wnf_eql_dry_dry e s a b
  _                -> wnf_eql_default e s a b

wnf_eql_set_set :: WnfEql
wnf_eql_set_set e s Set Set = do
  inc_inters e
  wnf e s Tru

wnf_eql_all_all :: WnfEql
wnf_eql_all_all e s (All aA aB) (All bA bB) = do
  inc_inters e
  wnf_enter e s (And (Eql aA bA) (Eql aB bB))

wnf_eql_lam_lam :: WnfEql
wnf_eql_lam_lam e s (Lam ax af) (Lam bx bf) = do
  inc_inters e
  x <- fresh e
  subst VAR e ax (Nam (int_to_name x))
  subst VAR e bx (Nam (int_to_name x))
  wnf_enter e s (Eql af bf)

wnf_eql_sig_sig :: WnfEql
wnf_eql_sig_sig e s (Sig aA aB) (Sig bA bB) = do
  inc_inters e
  wnf_enter e s (And (Eql aA bA) (Eql aB bB))

wnf_eql_tup_tup :: WnfEql
wnf_eql_tup_tup e s (Tup a1 a2) (Tup b1 b2) = do
  inc_inters e
  wnf_enter e s (And (Eql a1 b1) (Eql a2 b2))

wnf_eql_get_get :: WnfEql
wnf_eql_get_get e s (Get ac) (Get bc) = do
  inc_inters e
  wnf_enter e s (Eql ac bc)

wnf_eql_emp_emp :: WnfEql
wnf_eql_emp_emp e s Emp Emp = do
  inc_inters e
  wnf e s Tru

wnf_eql_efq_efq :: WnfEql
wnf_eql_efq_efq e s Efq Efq = do
  inc_inters e
  wnf e s Tru

wnf_eql_uni_uni :: WnfEql
wnf_eql_uni_uni e s Uni Uni = do
  inc_inters e
  wnf e s Tru

wnf_eql_one_one :: WnfEql
wnf_eql_one_one e s One One = do
  inc_inters e
  wnf e s Tru

wnf_eql_use_use :: WnfEql
wnf_eql_use_use e s (Use au) (Use bu) = do
  inc_inters e
  wnf_enter e s (Eql au bu)

wnf_eql_bol_bol :: WnfEql
wnf_eql_bol_bol e s Bol Bol = do
  inc_inters e
  wnf e s Tru

wnf_eql_fal_fal :: WnfEql
wnf_eql_fal_fal e s Fal Fal = do
  inc_inters e
  wnf e s Tru

wnf_eql_tru_tru :: WnfEql
wnf_eql_tru_tru e s Tru Tru = do
  inc_inters e
  wnf e s Tru

wnf_eql_fal_tru :: WnfEql
wnf_eql_fal_tru e s Fal Tru = do
  inc_inters e
  wnf e s Fal

wnf_eql_tru_fal :: WnfEql
wnf_eql_tru_fal e s Tru Fal = do
  inc_inters e
  wnf e s Fal

wnf_eql_if_if :: WnfEql
wnf_eql_if_if e s (If af at) (If bf bt) = do
  inc_inters e
  wnf_enter e s (And (Eql af bf) (Eql at bt))

wnf_eql_nat_nat :: WnfEql
wnf_eql_nat_nat e s Nat Nat = do
  inc_inters e
  wnf e s Tru

wnf_eql_zer_zer :: WnfEql
wnf_eql_zer_zer e s Zer Zer = do
  inc_inters e
  wnf e s Tru

wnf_eql_suc_suc :: WnfEql
wnf_eql_suc_suc e s (Suc a) (Suc b) = do
  inc_inters e
  wnf_enter e s (Eql a b)

wnf_eql_swi_swi :: WnfEql
wnf_eql_swi_swi e s (Swi az as) (Swi bz bs) = do
  inc_inters e
  wnf_enter e s (And (Eql az bz) (Eql as bs))

wnf_eql_lst_lst :: WnfEql
wnf_eql_lst_lst e s (Lst aT) (Lst bT) = do
  inc_inters e
  wnf_enter e s (Eql aT bT)

wnf_eql_nil_nil :: WnfEql
wnf_eql_nil_nil e s Nil Nil = do
  inc_inters e
  wnf e s Tru

wnf_eql_con_con :: WnfEql
wnf_eql_con_con e s (Con ah at) (Con bh bt) = do
  inc_inters e
  wnf_enter e s (And (Eql ah bh) (Eql at bt))

wnf_eql_mat_mat :: WnfEql
wnf_eql_mat_mat e s (Mat an ac) (Mat bn bc) = do
  inc_inters e
  wnf_enter e s (And (Eql an bn) (Eql ac bc))

wnf_eql_nam_nam :: WnfEql
wnf_eql_nam_nam e s (Nam x) (Nam y) = do
  inc_inters e
  if x == y then
    wnf e s Tru
  else
    wnf e s Fal

wnf_eql_dry_dry :: WnfEql
wnf_eql_dry_dry e s (Dry af ax) (Dry bf bx) = do
  inc_inters e
  wnf_enter e s (And (Eql af bf) (Eql ax bx))

wnf_eql_default :: WnfEql
wnf_eql_default e s a b = do
  wnf e s Fal