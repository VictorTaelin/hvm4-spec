wnf_unwind :: Env -> Stack -> Term -> IO Term
wnf_unwind e []      v = return v
wnf_unwind e (x : s) v = do
  when debug $ putStrLn $ "wnf_unwind: " ++ show (x:s) ++ " | " ++ show v
  case x of
    FApp a           -> wnf_app e s v a
    FDp0 k l         -> wnf_dup e s v k l (Dp0 k)
    FDp1 k l         -> wnf_dup e s v k l (Dp1 k)
    FAnd b           -> wnf_and e s v b
    FEql b           -> wnf_eql e s v b
    FEqlA a          -> wnf_eql_x e s a v
    FAppF f          -> wnf_app_f e s f v
    FAppG (Tup f a)  -> wnf_app_gua e s f v a
    FAppGF (Tup f m) -> wnf_app_gua_f e s f m v
    _                -> error "BadFrame"

