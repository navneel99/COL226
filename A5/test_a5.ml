#directory "_build";;
#load "a5.cmo";;
#load "a5gram.cmo";;
#load "a5lex.cmo";;

open A5;;
open A5gram;;
open A5lex;;

let parser s =  A5gram.a5_parser A5lex.read(Lexing.from_string s);;

let exprToClos e = Clos(e,[]);;

let expr1 = parser "\\X.(X+2)(\\Y.(Y+3)(4))";;

krivineMC (exprToClos expr1) [];;

