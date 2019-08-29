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

exception TypeError
exception Not_implemented

let p1 =  App (Lambda (V "x", Mult (Integer 3, V "x")), Integer 4);;
	(*12*)
let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 31), 
    Integer 0);;
   (*34*)
let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;
    (*110*)

let p4 = App(Lambda(V "x", App(Lambda(V "y", And(V "x", V "y")), Bool true)), Bool false);;
(*false*)

let p5 = App(Lambda(V "x", App(Lambda(V "y", Or(V "x", V "y")), Bool true)), Bool false);;
(*true*)

let p6 = App(Lambda(V "x", Mult(V "x", App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), Integer 3))), Integer 2);;
(*14*)

let p7 = If_Then_Else(Cmp(App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), Integer (-5))), Integer (-29), App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Plus(V "x", Integer 1)), Integer 7)), Integer 5)));;
(*13*)

let p8 = App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), App(Lambda(V "x", Mult(V "x", Integer 2)), Integer 3));;
(*10*)

let p9 = App(Lambda(V "x", App(Lambda(V "y", Mult(V "x", V "y")), V "x")), Integer 4);;
(*16*)

let p10 = App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Mult(V "x", Integer 2)), App(Lambda(V "x", Plus(V "x", Integer 4)), Integer 3)))), Integer 20);;
(*34*)

let p11 = App(Lambda(V "x", App(Lambda(V "y", And(V "x", V "y")), V "x")), Bool true);;
(*true*)

let p12 = If_Then_Else(Cmp(App(Lambda(V "x", Mult(V "x", Integer 2)), Integer 4)), App(Lambda(V "x", App(Lambda(V "y", Or(V "x", V "y")), V "x")), Bool false), Bool true);;
(*false*)

let p13 = App(Lambda(V "x", And(V "x", App(Lambda(V "x", And(V "x", Bool true)), App(Lambda(V "x", And(V "x", Bool true)), Bool true)))), Bool true);;
(*true*)

let p14 = App(Lambda(V "x", And(V "x", App(Lambda(V "x", And(V "x", Bool true)), App(Lambda(V "x", And(V "x", Bool true)), Bool true)))), Bool false);;
(*false*)

let p15 = If_Then_Else(Cmp(App(Lambda(V "x", Mult(V "x", App(Lambda(V "y", V "y"), V "x"))), Integer 1)), App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Plus(V "x", Integer 1)), Integer 3))), Integer 5), Integer (-1));;
(*9*)

let convertAnswerToExpr a = match a with
  | Aint b -> Integer b
  | Abool b -> Bool b
  | _ -> raise TypeError (* Careful *)

(*Your code will go here*)
(*For thise who have implemented lexer parser, modify the testcases in your grammar and you will have to get those tet_cases at the time of the demo*)

let eval_secd inp = let l = compile inp [] in
                    let ans = secdMC [] [] l [] in convertAnswerToExpr ans;;

let eval_krivine inp = let a1 = krivineMC (exprToClos inp) [] in let Clos(a,b) = a1 in a;;

(*Your code ends*)

let check_secd n inp out = let ans = eval_secd inp in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let check_krivine n inp out = let ans = eval_krivine inp in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let print_heading a = print_endline("\n" ^ a ^ " :");;

(*SECD*)
print_heading "SECD test cases\n";;

check_secd 1 (eval_secd p1) (Integer 12);;
check_secd 2 (eval_secd p2) (Integer 34);;
check_secd 3 (eval_secd p3) (Integer 110);;
check_secd 4 (eval_secd p4) (Bool false);;
check_secd 5 (eval_secd p5) (Bool true);;
check_secd 6 (eval_secd p6) (Integer 14);;
(* check_secd 7 (eval_secd p7) (Integer 13);; *)
check_secd 8 (eval_secd p8) (Integer 10);;
check_secd 9 (eval_secd p9) (Integer 16);;
check_secd 10 (eval_secd p10) (Integer 34);;
check_secd 11 (eval_secd p11) (Bool true);;
check_secd 12 (eval_secd p12) (Bool false);;
check_secd 13 (eval_secd p13) (Bool true);;
check_secd 14 (eval_secd p14) (Bool false);;
check_secd 15 (eval_secd p15) (Integer 9);; 

print_heading "Krivine test cases";;

check_krivine 1 (eval_secd p1) (Integer 12);;
check_krivine 2 (eval_secd p2) (Integer 34);;
check_krivine 3 (eval_secd p3) (Integer 110);;
check_krivine 4 (eval_secd p4) (Bool false);;
check_krivine 5 (eval_secd p5) (Bool true);;
check_krivine 6 (eval_secd p6) (Integer 14);;
(* check_krivine 7 (eval_secd p7) (Integer 13);; *)
check_krivine 8 (eval_secd p8) (Integer 10);;
check_krivine 9 (eval_secd p9) (Integer 16);;
check_krivine 10 (eval_secd p10) (Integer 34);;
check_krivine 11 (eval_secd p11) (Bool true);;
check_krivine 12 (eval_secd p12) (Bool false);;
check_krivine 13 (eval_secd p13) (Bool true);;
check_krivine 14 (eval_secd p14) (Bool false);;
check_krivine 15 (eval_secd p15) (Integer 9);; 
(*Krivine*)


