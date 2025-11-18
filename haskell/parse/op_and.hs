parse_op_and :: Term -> ReadP Term
parse_op_and t = do
  string "&&"
  t2 <- parse_term
  return (And t t2)