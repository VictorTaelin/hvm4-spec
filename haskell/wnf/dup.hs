wnf_dup :: Env -> Stack -> Term -> Name -> Lab -> Term -> IO Term
wnf_dup e s v k l t = do
  when debug $ putStrLn $ "wnf_dup: " ++ show (Dup k l v t)
  case v of
    Era    -> wnf_dup_era e s v k l t
    Sup {} -> wnf_dup_sup e s v k l t
    Set    -> wnf_dup_set e s v k l t
    All {} -> wnf_dup_all e s v k l t
    Lam {} -> wnf_dup_lam e s v k l t
    Nat    -> wnf_dup_nat e s v k l t
    Zer    -> wnf_dup_zer e s v k l t
    Suc {} -> wnf_dup_suc e s v k l t
    Swi {} -> wnf_dup_swi e s v k l t
    Nam {} -> wnf_dup_nam e s v k l t
    Dry {} -> wnf_dup_dry e s v k l t
    Gua {} -> wnf_dup_gua e s v k l t
    Sig {} -> wnf_dup_sig e s v k l t
    Tup {} -> wnf_dup_tup e s v k l t
    Get {} -> wnf_dup_get e s v k l t
    Emp    -> wnf_dup_emp e s v k l t
    Efq    -> wnf_dup_efq e s v k l t
    Uni    -> wnf_dup_uni e s v k l t
    One    -> wnf_dup_one e s v k l t
    Use {} -> wnf_dup_use e s v k l t
    Bol    -> wnf_dup_bol e s v k l t
    Fal    -> wnf_dup_fal e s v k l t
    Tru    -> wnf_dup_tru e s v k l t
    If {}  -> wnf_dup_if  e s v k l t
    Lst {} -> wnf_dup_lst e s v k l t
    Nil    -> wnf_dup_nil e s v k l t
    Con {} -> wnf_dup_con e s v k l t
    Mat {} -> wnf_dup_mat e s v k l t
    _      -> wnf_unwind e s (Dup k l v t)

wnf_dup_0 :: Env -> Stack -> Name -> Term -> Term -> IO Term
wnf_dup_0 e s k v t = do
  inc_inters e
  subst DP0 e k v
  subst DP1 e k v
  wnf e s t

wnf_dup_1 :: Env -> Stack -> Name -> Lab -> Term -> Term -> (Term -> Term) -> IO Term
wnf_dup_1 e s k l t v1 c = do
  inc_inters e
  (v1a, v1b) <- clone e l v1
  subst DP0 e k (c v1a)
  subst DP1 e k (c v1b)
  wnf e s t

wnf_dup_2 :: Env -> Stack -> Name -> Lab -> Term -> Term -> Term -> (Term -> Term -> Term) -> IO Term
wnf_dup_2 e s k l t v1 v2 c = do
  inc_inters e
  (v1a, v1b) <- clone e l v1
  (v2a, v2b) <- clone e l v2
  subst DP0 e k (c v1a v2a)
  subst DP1 e k (c v1b v2b)
  wnf e s t

wnf_dup_era :: WnfDup
wnf_dup_era e s Era k _ t = wnf_dup_0 e s k Era t

wnf_dup_sup :: WnfDup
wnf_dup_sup e s (Sup vl va vb) k l t
  | l == vl = do
      inc_inters e
      subst DP0 e k va
      subst DP1 e k vb
      wnf e s t
  | otherwise = do
      wnf_dup_2 e s k l t va vb (Sup vl)

wnf_dup_set :: WnfDup
wnf_dup_set e s Set k _ t = wnf_dup_0 e s k Set t

wnf_dup_all :: WnfDup
wnf_dup_all e s (All va vb) k l t = wnf_dup_2 e s k l t va vb All

wnf_dup_lam :: WnfDup
wnf_dup_lam e s (Lam vk vf) k l t = do
  inc_inters e
  x0      <- fresh e
  x1      <- fresh e
  (g0,g1) <- clone e l vf
  subst DP0 e k (Lam x0 g0)
  subst DP1 e k (Lam x1 g1)
  subst VAR e vk (Sup l (Var x0) (Var x1))
  wnf e s t

wnf_dup_nat :: WnfDup
wnf_dup_nat e s Nat k _ t = wnf_dup_0 e s k Nat t

wnf_dup_zer :: WnfDup
wnf_dup_zer e s Zer k _ t = wnf_dup_0 e s k Zer t

wnf_dup_suc :: WnfDup
wnf_dup_suc e s (Suc p) k l t = wnf_dup_1 e s k l t p Suc

wnf_dup_swi :: WnfDup
wnf_dup_swi e s (Swi vz vs) k l t = wnf_dup_2 e s k l t vz vs Swi

wnf_dup_nam :: WnfDup
wnf_dup_nam e s (Nam n) k _ t = wnf_dup_0 e s k (Nam n) t

wnf_dup_dry :: WnfDup
wnf_dup_dry e s (Dry vf vx) k l t = wnf_dup_2 e s k l t vf vx Dry

wnf_dup_gua :: WnfDup
wnf_dup_gua e s (Gua f g) k l t = wnf_dup_2 e s k l t f g Gua

wnf_dup_sig :: WnfDup
wnf_dup_sig e s (Sig a b) k l t = wnf_dup_2 e s k l t a b Sig

wnf_dup_tup :: WnfDup
wnf_dup_tup e s (Tup a b) k l t = wnf_dup_2 e s k l t a b Tup

wnf_dup_get :: WnfDup
wnf_dup_get e s (Get c) k l t = wnf_dup_1 e s k l t c Get

wnf_dup_emp :: WnfDup
wnf_dup_emp e s Emp k _ t = wnf_dup_0 e s k Emp t

wnf_dup_efq :: WnfDup
wnf_dup_efq e s Efq k _ t = wnf_dup_0 e s k Efq t

wnf_dup_uni :: WnfDup
wnf_dup_uni e s Uni k _ t = wnf_dup_0 e s k Uni t

wnf_dup_one :: WnfDup
wnf_dup_one e s One k _ t = wnf_dup_0 e s k One t

wnf_dup_use :: WnfDup
wnf_dup_use e s (Use u) k l t = wnf_dup_1 e s k l t u Use

wnf_dup_bol :: WnfDup
wnf_dup_bol e s Bol k _ t = wnf_dup_0 e s k Bol t

wnf_dup_fal :: WnfDup
wnf_dup_fal e s Fal k _ t = wnf_dup_0 e s k Fal t

wnf_dup_tru :: WnfDup
wnf_dup_tru e s Tru k _ t = wnf_dup_0 e s k Tru t

wnf_dup_if :: WnfDup
wnf_dup_if e s (If f tr) k l t = wnf_dup_2 e s k l t f tr If

wnf_dup_lst :: WnfDup
wnf_dup_lst e s (Lst x) k l t = wnf_dup_1 e s k l t x Lst

wnf_dup_nil :: WnfDup
wnf_dup_nil e s Nil k _ t = wnf_dup_0 e s k Nil t

wnf_dup_con :: WnfDup
wnf_dup_con e s (Con h tr) k l t = wnf_dup_2 e s k l t h tr Con

wnf_dup_mat :: WnfDup
wnf_dup_mat e s (Mat n c) k l t = wnf_dup_2 e s k l t n c Mat
