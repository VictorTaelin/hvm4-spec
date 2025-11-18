parse_lam_or_swi :: ReadP Term
parse_lam_or_swi = do
  lexeme (char 'λ')
  choice
    [ parse_lam_brace
    , parse_lam_var
    ]

parse_lam_brace :: ReadP Term
parse_lam_brace = do
  lexeme (char '{')
  t <- choice
    [ parse_get
    , parse_use
    , parse_if
    , parse_swi
    , parse_mat
    , return Efq
    ]
  lexeme (char '}')
  return t

parse_lam_var :: ReadP Term
parse_lam_var = do
  k <- parse_name
  lexeme (char '.')
  t <- parse_term
  return (Lam (name_to_int k) t)

parse_get :: ReadP Term
parse_get = do
  lexeme (char ',')
  lexeme (char ':')
  c <- parse_term
  optional (lexeme (char ';'))
  return (Get c)

parse_use :: ReadP Term
parse_use = do
  lexeme (string "()")
  lexeme (char ':')
  u <- parse_term
  optional (lexeme (char ';'))
  return (Use u)

parse_if :: ReadP Term
parse_if = do
  lexeme (string "#F")
  lexeme (char ':')
  f <- parse_term
  optional (lexeme (char ';'))
  lexeme (string "#T")
  lexeme (char ':')
  t <- parse_term
  optional (lexeme (char ';'))
  return (If f t)

parse_swi :: ReadP Term
parse_swi = do
  lexeme (char '0')
  lexeme (char ':')
  z <- parse_term
  optional (lexeme (char ';'))
  lexeme (string "1+")
  lexeme (char ':')
  s <- parse_term
  optional (lexeme (char ';'))
  return (Swi z s)

parse_mat :: ReadP Term
parse_mat = do
  lexeme (string "[]")
  lexeme (char ':')
  n <- parse_term
  optional (lexeme (char ';'))
  lexeme (string "<>")
  lexeme (char ':')
  c <- parse_term
  optional (lexeme (char ';'))
  return (Mat n c)