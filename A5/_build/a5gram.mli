type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | PLUS
  | CONJ
  | DISJ
  | IF
  | THEN
  | ELSE
  | FI
  | CMP
  | TIMES
  | BACKSLASH
  | DOT
  | LP
  | RP
  | SEMICOLON
  | EOF

val a5_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A5.expr
