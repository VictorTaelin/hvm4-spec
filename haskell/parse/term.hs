parse_term :: ReadP Term
parse_term = do
  t <- parse_term_base
  parse_term_suff t

parse_term_suff :: Term -> ReadP Term
parse_term_suff t = skipSpaces >> choice
  [ parse_op_and t
  , parse_op_eql t
  , parse_op_gua t
  , parse_op_lst t
  , parse_op_con t
  , return t
  ]

parse_term_base :: ReadP Term
parse_term_base = lexeme $ choice
  [ parse_lam_or_swi
  , parse_dup
  , parse_par
  , parse_sup
  , parse_era
  , parse_set
  , parse_all
  , parse_sig
  , parse_nat
  , parse_add
  , parse_num
  , parse_ref
  , parse_emp
  , parse_uni
  , parse_bol
  , parse_ctr
  , parse_nil
  , parse_nam
  , parse_var
  ]