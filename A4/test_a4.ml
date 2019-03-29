#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";;
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "a4.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;
open A4;;

exception Not_implemented
(* Helper function to print *)
let rec print_tree tr = match tr with
  N a -> "INT " ^ (string_of_int a)
  | _ -> raise Not_implemented
;;
let rec print_answer tr = match tr with
  Num a -> print_num a
  | Bool a -> string_of_bool a
  | _ -> raise Not_implemented
;;
let rec print_value tr = match tr with
  NumVal a -> string_of_int a
  | BoolVal a -> string_of_bool a
  | _ -> raise Not_implemented
;;
let rec print_def df = match df with
  Simple(l,r) -> "def " ^ l ^ " = " ^ (print_tree r)
  | _ -> raise Not_implemented
;;


(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;

(* Input is given as string and output is an answer *)
let binding rho s = toAnswer (rho s);;

(* Both use the same lexer in A1 but different parser in A3 *)
let exp_parser s rho = A3.exp_parser A2.read (Lexing.from_string s) ;;
let def_parser s rho = A3.def_parser A2.read (Lexing.from_string s) ;;

(* Input is given as string and output is a value *)
let rho s = match s with
  "X" -> NumVal 5
  |  "Y" -> BoolVal true
  |  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1])
  | _ -> raise Not_implemented
;;

(* Sample parsing *)
(* print_endline ( print_tree (exp_parser "5" rho));;
print_endline ( print_def (def_parser "def A=5" rho));; *)


(* Sample test case *)
(* let e = (exp_parser "2+3" rho);;
let t = Tfunc (Tint, Tbool);; *)




(* Type assumptions as a list of tuples of the form (variable name, type) *)
(* let g = [("X", Tint); ("Y", Tbool); ("Z", Ttuple [Tint ; Tbool ; Tint]); ("W", Tfunc (Tint, Tbool))];; *)
(* let d = (def_parser " def U=X ;local (def M = W) in (def N = M) end" rho);; *)
let g = [("X", Tint); ("Y", Tbool); ("Z", Tfunc(Tint, Tbool)); ("W", Tfunc(Tbool, Tint))]
let g_dash = [("U", Tint); ("V", Tbool)];;

let e1 = exp_parser "let def A = Z(X) in W(A) end" rho;;
hastype g e1 Tint;;              (* should return true *)
let e2 = exp_parser "\\M.(W(Z(M)))" rho;;
hastype g e2 (Tfunc(Tint, Tint));; (* should return true *)


let d1 = def_parser "def U = X" rho;;
let d2 = def_parser "def U = X ; def V = W(Y)" rho;;
let d3 = def_parser "def V = W(X)" rho;;

yields g d1 [("U", Tint)];;              (* should return true *)
yields g d2 [("U", Tint); ("V", Tint)];; (* should return true *)
yields g d3 [("V", Tint)];;              (* should return false because of failed type check *)

(* 
assert(hastype g e t);;
assert(yields g d g_dash);; *)
(* hastype g e t;;
yields g d g_dash;; *)
