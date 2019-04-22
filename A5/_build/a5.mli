type expr =
   V of string 
   | Lambda of (expr * expr) 
   | App of (expr * expr) 
   | Plus of (expr * expr) 
   | Mult of (expr * expr) 
   | And of (expr * expr) 
   | Or of (expr * expr) 
   | Bool of bool 
   | Integer of int 
   | Cmp of expr 
   | If_Then_Else of (expr * expr * expr);;


type opcode = VID of string | APP | PLUS | MULT | AND | OR 
  |BOOL of bool | INT of int | CMP | COND of (opcode list * opcode list) 
  |CLOS of (opcode list * opcode list) |RET;;

type table = (expr * closure) list
and closure = Clos of expr * table;;

type opclosure = (string * opcode list * table);;

type answer = Aint of int | Abool of bool| Avcl of opclosure;;

val krivineMC: closure -> closure list -> closure 

val compile: expr -> 'a -> opcode list 

val secdMC: answer list -> table -> opcode list -> (answer list * table * opcode list) list -> answer
