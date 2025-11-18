parse_op_con :: Term -> ReadP Term
parse_op_con t = do
  string "<>"
  t2 <- parse_term
  return (Con t t2)