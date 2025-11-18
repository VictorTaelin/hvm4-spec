parse_op_gua :: Term -> ReadP Term
parse_op_gua t = do
  string "~>"
  t2 <- parse_term
  return (Gua t t2)