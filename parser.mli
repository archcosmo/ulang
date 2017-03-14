type token =
  | INT of (int)
  | IN of (int)
  | WORD of (string)
  | LBRACE
  | COMMA
  | RBRACE
  | KLEENESTAR
  | UNION
  | INTERSECT
  | CONCATENATION
  | DIFFERENCE
  | COMPLEMENT
  | EMPTYWORD
  | EOF

val parser_main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ULang.uTerm
