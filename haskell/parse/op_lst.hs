parse_op_lst :: Term -> ReadP Term
parse_op_lst t = do
  string "[]"
  parse_term_suff (Lst t)