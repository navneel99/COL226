{
  open A5gram
  exception Not_implemented
  exception InvalidToken of char;;
}

let integer = ('0'|['1'-'9']+['0'-'9']*)

let whitespace =  [' ' '\t']+

let str = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_' '\'']*

rule read = parse
  | eof   { EOF }
  | integer as n {INT (int_of_string (n)) }
  | whitespace {read lexbuf}
  | "T" {BOOL true}
  | "F" {BOOL false}
  | "+" {PLUS}
  | "*" {TIMES}
  | "/\\" {CONJ}
  | "\\/" {DISJ}
  | "cmp" {CMP}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "fi"  {FI}
  | "\\" {BACKSLASH}
  | ";" {SEMICOLON}
  | "." {DOT}
  | "(" {LP}
  | ")" {RP}
  | str as a {ID a}
  | _ as a {raise (InvalidToken a)}