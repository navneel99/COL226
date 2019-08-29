#directory "_build";;
#load "a6.cmo";;
#load "a6gram.cmo";;
#load "a6lex.cmo";;
open A6;;
open A6gram;;
open A6lex;;

let rec testing a = 
    let () = print_string "Wow"in
    let ans = read_line () in
    let () = print_string ans in
    testing "a";;

let rec get_procedures sf = match sf with
  | [] -> []
  | x::xs -> let Frame(Procedure(p),a,l,al,sl) = x in p::get_procedures xs

let parser s = A6gram.a6_parser A6lex.read(Lexing.from_string s);;

let rec print_Var l =   match l with
  | [] -> let () = print_string "\n" in ()
  | x::xs ->  let ArgI(a,b) = x in let () = print_string a in 
              let () = print_string ":=" in let () = print_int b in
              let () = print_string "\n" in print_Var xs
let rec print_SL l = match l with
  | [] -> []
  | x::xs -> let Procedure(p) = x in p::print_SL xs

let rec interpreter sf =
    let procs = get_procedures sf in
    let () = print_string "----------------------------------------\n" in
    let () = print_string (String.concat " :: " (procs)) in
    let () = print_endline "\n" in
    let () = print_string "----------------------------------------\n" in
    let () = print_string "Current Stack is shown above. What would you like to do? \n" in
    let () = print_string "1)Console Commands\n2)Show Local Variables\n3)Show Arguments\n4)Show Static Link\n5)Show All Variables\n" in
    let what = read_line () in
      if (what = "1") then
        let () = print_string "----------------------------------------\n" in
        let () = print_string "Enter your command below: \n" in
        let command = read_line () in
        let exp1 = parser command in
        if (command = "return") then
            let ans = return sf in interpreter ans
        else
        let ans = (match exp1 with
            | Frame(p,a,l,al,sl) -> addToFrame sf exp1
            | ArgI(a,b) -> assignValue sf exp1
            | ArgS(a,c) -> assignValue sf exp1
            | _ -> raise (TypeError "Wrong Input")) 
        in interpreter ans
      else if (what = "2") then
        let () = print_string "----------------------------------------\n" in
        let () = print_string "Local Variables for the stack frame head is: \n" in
        let l = returnLocalV sf in
        let () = print_Var l in
        interpreter sf
      else if (what = "3") then
        let () = print_string "----------------------------------------\n" in
        let () = print_string "Its arguments are:\n" in
        let Frame(p,a,l,al,sl) = List.hd sf in 
        let ArgI(a1,b1),ArgI(a2,b2) = a in
        let () = print_string a1 in let () = print_string ":=" in let () = print_int b1 in let () = print_string "\n" in
        let () = print_string a2 in let () = print_string ":=" in let () = print_int b2 in let () = print_string "\n" in
        interpreter sf
      else if (what = "4") then
        let () = print_string "----------------------------------------\n" in
        let () = print_string "The Static Link Chain is: \n" in
        let  Frame(p,a,l,al,FrameList sl) = List.hd sf in
        let staticl = print_SL sl in     
        let () = print_string (String.concat " ; " (staticl)) in
        let () = print_string "\n" in
        interpreter sf
      else if (what = "5") then
        let () = print_string "----------------------------------------\n" in
        let () = print_string " All the variables that the stack head can access are: \n" in
        let al= returnAllV sf in
        let () = print_Var al in interpreter sf
      else  let () = print_string "Please Enter a correct option:\n\n" in interpreter sf;;

interpreter [];;