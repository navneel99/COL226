{
  type token =
   INT of int          (* integer constant, positive or negative w/o leading zeros *)
|  TRUE                (* boolean constant "T" *)
|  FALSE               (* boolean constant "F" *)
|  ABS                 (* unary operator, "abs" *)
|  PLUS                (* arithmetic plus, "+" *)
|  MINUS               (* arithmetic minus, "-" *)
|  MUL                 (* arithmetic multiply, "*" *)
|  DIV                 (* integer div, "div" *)
|  MOD                 (* remainder, "mod" *)
|  EXP                 (* exponentiation, "^" *)
|  LP                  (* left paren, "(" *)
|  RP                  (* right paren, ")" *)
|  NOT                 (* boolean NOT, "not" *)
|  AND                 (* boolean AND, "/\ " *)
|  OR                  (* boolean OR, "\/" *)
|  EQ                  (* equal to, "=" *)
|  GTA                 (* greater than, ">" *)
|  LTA                 (* less than, "<" *)
|  GEQ                 (* greater than/equal to, ">=" *)
|  LEQ                 (* less than/equal to, "<=" *)
|  IF                  (* keyword "if" *)
|  THEN                (* keyword "then" *)
|  ELSE                (* keyword "else" *)
|  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
|  DEF                 (* definition construct, "def" *)
|  DELIMITER;;         (* delimiter, ";" *)

exception InvalidToken of char;;

let remPlus a = let b = (String.length a) in
             let c = a.[0] in 
if (c = '+') then (String.sub a 1 (b-1)) else a;;

}

let integer = ('0'| ('+'|'-')?['1'-'9']+['0'-'9']*)
let whitespace =  [' ' '\t']+
let str = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*

rule read  = parse
  | integer as n {INT (int_of_string (remPlus n)) :: read lexbuf}
  | whitespace {read lexbuf}
  | "T" {TRUE :: read lexbuf}
  | "F" {FALSE :: read lexbuf}
  | "abs" {ABS :: read lexbuf}
  | "+" {PLUS :: read lexbuf}
  | "-" {MINUS :: read lexbuf}
  | "*" {MUL :: read lexbuf}
  | "div" {DIV :: read lexbuf}
  | "mod" {MOD :: read lexbuf}
  | "^" {EXP :: read lexbuf}
  | "(" {LP :: read lexbuf}
  | ")" {RP :: read lexbuf}
  | "not" {NOT :: read lexbuf}
  | "/\\" {AND :: read lexbuf}
  | "\\/" {OR :: read lexbuf}
  | "=" {EQ :: read lexbuf}
  | ">" {GTA :: read lexbuf}
  | "<" {LTA :: read lexbuf}
  | ">=" {GEQ :: read lexbuf}
  | "<=" {LEQ :: read lexbuf}
  | "if" {IF :: read lexbuf}
  | "then" {THEN :: read lexbuf}
  | "else" {ELSE :: read lexbuf}
  | "def" {DEF :: read lexbuf}
  | str as a {ID a :: read lexbuf}
  | ";" {DELIMITER :: read lexbuf}
  | eof {[]}
  | _ as a {raise (InvalidToken a)}



{
  let scanner s = read (Lexing.from_string s)
}

(* 
examples:
scanner "00" identifies as 2 separate 0s resulting in :token list = [INT 0; INT 0]
scanner "123 456" identifies correctly as 2 INT
scanner "T F" identifies as TRUE and FALSE
scanner "+-*^div mod" is  token list = [PLUS; MINUS; MUL; EXP; DIV; MOD]
scanner "52-5;52 - 5" gives token list = [INT 52; INT (-5); DELIMITER; INT 52; MINUS; INT 5]
scanner ")( not = > < >= <= if then else" gives token list = [RP; LP; NOT; EQ; GTA; LTA; GEQ; LEQ; IF; THEN; ELSE]
scanner "aBC0922" gives token list = [ID "aBC0922"]
scanner "def dEf ddeeff" gives token list = [DEF; ID "dEf"; ID "ddeeff"]

counterexamples:
writing scanner ";* /\\/ 52 - 9(;;;)" gives Exception: Failure "lexing: empty token".
as "or after and" makes \\ as the escape character causing problems. 
Adding a whitespace takes care of the problem.
There is no such issue with "and after or".
*)





 