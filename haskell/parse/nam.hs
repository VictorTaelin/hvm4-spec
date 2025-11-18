parse_nam :: ReadP Term
parse_nam = do
  lexeme (char '^')
  choice
    [ do lexeme (char '('); f <- parse_term; x <- parse_term; lexeme (char ')'); return (Dry f x)
    , do k <- parse_name; return (Nam k)
    ]