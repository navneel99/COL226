{
  open A6gram
  exception Not_implemented
  exception InvalidToken of char;;
}


let integer = ('0'|['1'-'9']+['0'-'9']*)

let whitespace =  [' ' '\t']+

let str = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_' '\'']*

let variable = ['a'-'z']['a'-'z''A'-'Z''0'-'9''_' '\'']*

rule read = parse
  | eof   { EOF }
  | whitespace {read lexbuf}
  | "(" {LP}
  | ")" {RP}
  | "," {COMMA}
  | ":" {COLON}
  | "=" {EQ}
  | "return" {RET}
  | str as a {FNAME a}
  | variable as a {ID a}
  | integer as a {INT (int_of_string (a))}
  | _ as a {raise (InvalidToken a)}