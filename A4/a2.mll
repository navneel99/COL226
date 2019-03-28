{
  open A3
  exception Not_implemented
  exception InvalidToken of char;;

let remPlus a = let b = (String.length a) in
             let c = a.[0] in 
if (c = '+') then (String.sub a 1 (b-1)) else a;;
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
  - This is sample implementation. Please rewrite them as per the specifications
*)

let integer = ('0'|['1'-'9']+['0'-'9']

let whitespace =  [' ' '\t']+
let str = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_' '\'']*

rule read = parse
   eof                { EOF }
  | integer as n {INT (int_of_string (remPlus n)) }
  | whitespace {read lexbuf}
  | "T" {BOOL true}
  | "F" {BOOL false}
  | "abs" {ABS}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {TIMES}
  | "div" {DIV}
  | "mod" {REM}
  | "~" {TILDA}
  | "(" {LP}
  | ")" {RP}
  | "not" {NOT}
  | "/\\" {CONJ}
  | "\\/" {DISJ}
  | "=" {EQ}
  | ">" {GT}
  | "<" {LT}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "fi"  {FI}
  | "," {COMMA}
  | "proj" {PROJ}
  | "def" {DEF}
  | "in" {IN}
  | "end" {END}
  | "\\" {BACKSLASH}
  | ";" {SEMICOLON}
  | "." {DOT}
  | "||" {PARALLEL}
  | "local" {LOCAL}
  | str as a {ID a}
  | _ as a {raise (InvalidToken a)}