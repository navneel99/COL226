type token =
  | ID of (string)
  | INT of (int)
  | FNAME of (string)
  | LP
  | RP
  | COMMA
  | COLON
  | EQ
  | RET
  | EOF

val a6_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A6.arg
