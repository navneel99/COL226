open A1
exception Not_implemented
exception TypeError
(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
(* let rec hastype g e t = raise Not_implemented *)

(* Function 'hastype' takes a set of type assumptions G represented as a list
of tuples of form (variable name, type), an expression and an expression
type, and returns if the expression has the claimed type under the given
assumptions. *)

let rec getTypeFromList g e = match g with
                                | [] -> raise TypeError
                                | x::xs -> let a,b = x in if (e = a) then b else getTypeFromList xs e 

let rec ithFromList l n = match n,l with
                            | 0,x::xs -> x
                            | i,x::xs -> ithFromList xs (i-1) 


let rec hastype g e t = let rec retFun g e = (match e with
                          | N x -> Tint
                          | B x -> Tbool
                          | Var s -> getTypeFromList g s
                          | Abs x | Negative x | Not x-> retFun g x 
                          | Add(x,y) | Sub(x,y) | Mult(x,y) | Div(x,y) | Rem(x,y) -> Tint
                          | Conjunction(x,y) | Disjunction(x,y) | Equals(x,y) | GreaterT(x,y) | GreaterTE(x,y) | LessT(x,y) | LessTE(x,y) -> Tbool
                          | InParen x -> retFun g x 
                          | IfThenElse(x,y,z) -> let ret1 = retFun g y in let ret2 = retFun g z in
                                                  if (ret1=ret2) then ret1 else ret2 (* WRONG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
                          | Tuple(n,l) -> let rec listAdder l =  
                                                          (match l with
                                                          | [] -> []
                                                          | x::xs -> let temp = retFun g x in temp::listAdder xs)  
                                                        in let temp2 = listAdder l in Ttuple(temp2) 
                          | Project(a,l) -> let temp = retFun g l in let i,n = a in (match temp with 
                                                                                     | Ttuple x -> ithFromList x i
                                                                                     | _ -> raise TypeError)
                          | FunctionAbstraction(s,l) -> let temp = getTypeFromList g s in let temp2 = retFun g l in Tfunc(temp, temp2)
                          | FunctionCall(a,b) -> let temp = retFun g a in let temp2 = retFun g b in Tfunc(temp, temp2))
                        in let temp3 = retFun g e  in if (temp3 = t) then true else false

(* yields : ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool *)
(* let rec yields g d g_dash = raise Not_implemented *)

(* Function 'yields' takes a set of type assumptions G, a definition d and
another set of type assumptions G', and decides whether under the given
assumptions G, the definition d yields the type assumptions G' or not. *)

