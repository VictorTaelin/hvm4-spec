parse_op_eql :: Term -> ReadP Term
parse_op_eql t = do
  string "=="
  t2 <- parse_term
  return (Eql t t2)