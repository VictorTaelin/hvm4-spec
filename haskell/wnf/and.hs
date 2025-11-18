wnf_and :: Env -> Stack -> Term -> Term -> IO Term
wnf_and e s a b =  do
  case a of
    Era    -> wnf_and_era e s a b
    Sup {} -> wnf_and_sup e s a b
    Fal    -> wnf_and_fal e s a b
    Tru    -> wnf_and_tru e s a b
    _      -> wnf_unwind e s (And a b)

wnf_and_era :: Env -> Stack -> Term -> Term -> IO Term
wnf_and_era e s Era b = do
  inc_inters e
  wnf e s Era

wnf_and_sup :: Env -> Stack -> Term -> Term -> IO Term
wnf_and_sup e s (Sup l a0 a1) b = do
  inc_inters e
  (b0, b1) <- clone e l b
  wnf_enter e s (Sup l (And a0 b0) (And a1 b1))

wnf_and_fal :: Env -> Stack -> Term -> Term -> IO Term
wnf_and_fal e s Fal b = do
  inc_inters e
  wnf e s Fal

wnf_and_tru :: Env -> Stack -> Term -> Term -> IO Term
wnf_and_tru e s Tru b = do
  inc_inters e
  wnf e s b