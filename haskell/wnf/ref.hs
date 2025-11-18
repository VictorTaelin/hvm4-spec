wnf_ref :: Env -> Stack -> Name -> IO Term
wnf_ref e s k = do
  let (Book m) = env_book e
  case M.lookup k m of
    Just f  -> do
      inc_inters e
      g <- alloc e f
      wnf_enter e s g
    Nothing -> error $ "UndefinedReference: " ++ int_to_name k