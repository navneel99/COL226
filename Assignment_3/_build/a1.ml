(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception TypeError
exception TupleError of string

(* abstract syntax *)
type  exptree =  
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

let rec eleFinder l i  = match l,i with
    | x::xs,1 -> x
    | x::xs,y -> eleFinder xs (y-1)

let rec listFunc l f a = match l with
                    | [] -> []
                    | x::xs -> (f x a)::(listFunc xs f a)
(* let typeChecker p ty f = (match p with
                            | ty(a) -> ty(f a)  
                            | _ -> raise TypeError) *)

let rec eval ex rho = match ex with 
    |   N x -> NumVal (x)
    |   B x -> BoolVal (x)
    |   Var s -> rho s
    |   Negative x -> let a = eval x rho in 
                                            (match a with
                                              | NumVal(b) -> NumVal(-b)
                                              | _ -> raise TypeError)  
    |   Abs x -> let a = eval x rho in      (match a with
                                              | NumVal(b) -> NumVal(Pervasives.abs b)
                                              | _ -> raise TypeError)                                              
    |   Not x ->  let a = eval x rho in 
                                        (match a with
                                          | BoolVal(r) -> BoolVal(not r)
                                          | _ -> raise TypeError )
    |   Add(x,y) ->  let  a = (eval x rho) in let  b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> NumVal(s1 + s2)
                                                                  | _ -> raise TypeError)
    |   Sub(x,y) ->  let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> NumVal(s1-s2)
                                                                  | _ -> raise TypeError)
    |   Mult(x,y) -> let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> NumVal(s1 * s2)
                                                                  | _ -> raise TypeError) 
    (* mult (eval x rho) (eval y rho) *)
    |   Div(x,y) ->  let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> NumVal(s1/s2)
                                                                  | _ -> raise TypeError)
    (* div (eval x rho) (eval y rho) *)
    |   Rem(x,y) -> let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> NumVal(s1 mod s2)
                                                                  | _ -> raise TypeError)
     (* rem (eval x rho) (eval y rho) *)
    |   Conjunction(x,y) -> let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | BoolVal(s1),BoolVal(s2) -> BoolVal(s1 && s2)
                                                                  | _ -> raise TypeError)
    (* (eval x rho) || (eval y rho) *)
    |   Disjunction(x,y) -> let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | BoolVal(s1),BoolVal(s2) -> BoolVal(s1 || s2)
                                                                  | _ -> raise TypeError)
    (* (eval x rho) && (eval y rho) *)
    |   Equals(x,y) -> let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> BoolVal(s1 = s2)
                                                                  | _ -> raise TypeError)
    (* eq (eval x rho) (eval y rho) *)
    |   GreaterTE(x,y) -> let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> BoolVal(s1 >= s2)
                                                                  | _ -> raise TypeError)
    (* geq (eval x rho) (eval y rho) *)
    |   LessTE(x,y) -> let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> BoolVal(s1 <= s2)
                                                                  | _ -> raise TypeError)
    (* leq (eval x rho) (eval y rho) *)
    |   GreaterT(x,y) -> let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> BoolVal(s1 > s2)
                                                                  | _ -> raise TypeError)
    (* gt (eval x rho) (eval y rho) *)
    |   LessT(x,y) -> let  a = (eval x rho) in let b = (eval y rho)  in 
                                                                  (match a,b with
                                                                  | NumVal(s1),NumVal(s2) -> BoolVal(s1 < s2)
                                                                  | _ -> raise TypeError)
    (* lt (eval x rho) (eval y rho) *)
    |   InParen x -> eval x rho
    |   IfThenElse(x,y,z) -> let a = eval x rho in
                             (if (a = BoolVal true ) then (eval y rho) else (eval z rho))
    |   Tuple(n,l) -> let te = n in
                        (let r = (listFunc l eval rho) in TupVal(te,r))
    |   Project(a,b) -> let i,n = a in if (i<=n) then (match b with 
                                          | Tuple(w,l) -> if (w = n) then let z1 = eleFinder l i in eval z1 rho else raise(TupleError "The lengths of tuple don't match.")
                                          | _ -> raise TypeError) 
                                          else raise(TupleError "Your element is greater than the list")                  

(* opcodes of the stack machine (in the same sequence as above) *)


let rec indivApp f a = begin match a with
  | [] -> []
  | y::ys -> (indivApp f ys) @ (f y)
end

type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int;;

  let rec compile t = match t with
  |   N x -> let y = mk_big(x) in [NCONST y]
  |   B x -> [BCONST x]
  |   Var s ->[VAR s]
  |   Add(x,y) -> (compile y) @ (compile x) @ [PLUS]
  |   Sub(x,y) ->(compile y) @ (compile x) @ [MINUS]
  |   Mult(x,y) -> (compile y) @ (compile x) @ [MULT]
  |   Div(x,y) -> (compile y) @ (compile x) @ [DIV]
  |   Rem(x,y) -> (compile y) @ (compile x) @ [REM]
  |   Negative x -> (compile x) @ [UNARYMINUS]
  |   Abs x  -> (compile x) @ [ABS]
  |   Not x -> (compile x) @ [NOT]
  |   Conjunction(x,y) -> (compile y) @ (compile x) @ [CONJ]
  |   Disjunction(x,y) -> (compile y) @ (compile x) @ [DISJ]
  |   Equals(x,y) -> (compile y) @ (compile x) @ [EQS]
  |   GreaterTE(x,y) -> (compile y) @ (compile x) @ [GTE]
  |   LessTE(x,y) -> (compile y) @ (compile x) @ [LTE] 
  |   GreaterT(x,y) -> (compile y) @ (compile x) @ [GT]
  |   LessT(x,y) -> (compile y) @ (compile x) @ [LT]
  |   InParen x -> (compile x) @ [PAREN]
  |   IfThenElse(x,y,z) -> (compile z)@(compile y)@(compile x)@[IFTE]
  |   Tuple(x,l) -> let a = indivApp compile l in a @ [TUPLE x]
  |   Project(a,b) ->  let i,n = a in (match b with
                        | Tuple(x1,x2) -> if (x1 = n) then let y = (indivApp compile x2) in y @ [PROJ (i,n)] else raise(TupleError "Lengths of tuple don't match.")
                        | _ -> raise (TupleError "2nd argument is not a tuple."))


let sinopuse a b = match a with
    |   UNARYMINUS -> (match b with
                        | Num(k) -> Num(minus k)
                        | _ -> raise TypeError)
    |   ABS -> (match b with
                  | Num(k) -> Num(abs k)
                  | _ -> raise TypeError)
    |   NOT -> (match b with
                | Bool(k) -> Bool(not k)
                | _ -> raise TypeError)
    |   PAREN -> b

let twoopuse a b c = match a with
    |   PLUS -> (match b,c with
                | Num(x),Num(y) -> Num(add x y)
                | _,_ -> raise TypeError)
    |   MINUS -> (match b,c with
                | Num(x),Num(y) -> Num(sub x y)
                | _,_ -> raise TypeError)
    |   MULT -> (match b,c with
                  | Num(x),Num(y) -> Num(mult x y)
                  | _,_ -> raise TypeError) 
    |   DIV -> (match b,c with
                  | Num(x),Num(y) -> Num(div x y)
                  | _,_ -> raise TypeError)
    |   REM -> (match b,c with
                  | Num(x),Num(y) -> Num(rem x y)
                  | _,_ -> raise TypeError)
    |   CONJ -> (match b,c with
                  | Bool(x),Bool(y) -> Bool(x && y)
                  | _,_ -> raise TypeError)
    |   DISJ -> (match b,c with
                  | Bool(x),Bool(y) -> Bool(x || y)
                  | _,_ -> raise TypeError)
    |   EQS -> (match b,c with
                  | Num(x),Num(y) -> Bool(eq x y)
                  | _,_ -> raise TypeError)
    |   GTE -> (match b,c with
                  | Num(x),Num(y) -> Bool(geq x y)
                  | _,_ -> raise TypeError)
    |   LTE -> (match b,c with
                  | Num(x),Num(y) -> Bool(leq x y)
                  | _,_ -> raise TypeError)
    |   GT  -> (match b,c with
                  | Num(x),Num(y) -> Bool(gt x y)
                  | _,_ -> raise TypeError)
    |   LT  -> (match b,c with
                  | Num(x),Num(y) -> Bool(lt x y)
                  | _,_ -> raise TypeError)


let rec listPopper l n = match n,l with
                          | 0,_ -> []
                          | x,y::ys -> y::(listPopper ys (x-1)) 
let rec remListAfterPopping l n = match n,l with
                          | 0,d -> d
                          | x,y::ys -> remListAfterPopping ys (x-1) 

let rec stackmc bl rho ol = match ol with
    |   [] -> List.hd(bl)
    |   x::xs -> match x with 
                | VAR s -> stackmc ((rho s)::bl) rho xs
                | NCONST y -> stackmc (Num(y) :: bl) rho xs
                | BCONST y -> stackmc (Bool(y) :: bl) rho xs
                | UNARYMINUS | ABS | NOT | PAREN -> let fir_ele = List.hd(bl) and rem_list = List.tl(bl) in
                                        let ans = sinopuse x fir_ele in 
                                        stackmc (ans :: rem_list) rho xs
                | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT -> (match bl with h::m::t -> let ans = twoopuse x h m in
                                                                                 stackmc (ans :: t) rho xs)
                | IFTE -> (match bl with fe::se::te::tl -> if (fe = Bool true) then (stackmc (se::tl) rho xs) else (stackmc (te::tl) rho xs) )
                | TUPLE n -> (let popdEle = listPopper bl n in let leftOver = remListAfterPopping bl n in
                          stackmc (Tup(n,popdEle)::leftOver) rho xs)
                | PROJ(i,n) -> if (i<=n) then let popdEle = listPopper bl n in let leftOver = remListAfterPopping bl n in 
                          stackmc ((eleFinder popdEle i)::leftOver) rho xs
                            else raise(TupleError "Element to find is greater than the list of elements")
