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

let rec eleInL n l = match l with
                            | [] -> false
                            | x::xs -> if (n = x) then true else eleInL n xs
                          
                          (* Wont work in case of duplicates *)
let rec chk2L l1 l2 = match l1 with
                            | [] -> true
                            | x::xs -> let temp=eleInL x l2 in if (temp = false) then false else chk2L xs l2

let rec shouldFun g e =  match e with
                        | N x -> Tint
                        | B x -> Tbool
                        | Var s -> getTypeFromList g s 
                        | Abs x | Negative x -> shouldFun g x 
                        | Not x -> shouldFun g x
                        | Add(x,y) | Sub(x,y) | Mult(x,y) | Div(x,y) | Rem(x,y)  ->  Tint
                        | Conjunction(x,y) | Disjunction(x,y) | Equals(x,y) | GreaterT(x,y) | GreaterTE(x,y) | LessT(x,y) | LessTE(x,y)-> Tbool
                        | InParen x -> shouldFun g x 
                        | IfThenElse(x,y,z) -> let ret1 = shouldFun g y in let ret2 = shouldFun g z in ret1
                        | Tuple(n,l) -> let rec listAdder l =  
                                                  (match l with
                                                  | [] -> []
                                                  | x::xs -> let temp = shouldFun g x in temp::listAdder xs)  
                                                in let temp2 = listAdder l in Ttuple(temp2) 
                        | Project(a,l) -> let temp = shouldFun g l in let i,n = a in (match temp with 
                                                                                    | Ttuple x -> ithFromList x i
                                                                                    | _ -> raise TypeError)
                        | FunctionAbstraction(s,l) -> let temp2 = shouldFun g l in let pred = shouldFun g (Var s) in Tfunc(pred,temp2)
                        | FunctionCall(a,b) -> let temp = shouldFun g a in let temp2 = shouldFun g b in (match temp,temp2 with 
                                                                                                    | Tfunc(a,b),Tfunc(c,d) -> Tfunc(c,b)
                                                                                                    | Tfunc(a,b),c -> b
                                                                                                    | _ -> raise TypeError)

(* let rec lastTry g e vbl = match e with
    | Abs x | Negative x -> (match x with 
                            | Var s -> if (s = vbl) then Tint
                            | _ -> lastTry g x vbl )
    | Not x -> (match x with
                | Var s -> if (s=vbl) then Tbool
                | _ -> lastTry g x vbl )
    | Add(x,y) | Sub(x,y) | Mult(x,y) | Div(x,y) | Rem(x,y) -> (match x,y with
                                                                | Var s1, Var s2 -> if (s1 = vbl) || (s2 = vbl) then Tint
                                                                | Var s1,_ -> if (s1 = vbl) then Tint
                                                                | _, Var s1 -> if (s1 = vbl) then Tint 
                                                                | _,_ -> lastTry g x vbl)
    |  Equals(x,y) | GreaterT(x,y) | GreaterTE(x,y) | LessT(x,y) | LessTE(x,y) -> (match x,y with
                                                                                    | Var s1, Var s2 -> if (s1 = vbl) || (s2 = vbl) then Tbool
                                                                                    | Var s1,_ -> if (s1 = vbl) then Tbool
                                                                                    | _, Var s1 -> if (s1 = vbl) then Tbool 
                                                                                    | _,_ -> lastTry g x vbl )
    | Conjunction(x,y) | Disjunction(x,y) -> (match x,y with
                                                | Var s1, Var s2 -> if (s1 = vbl) || (s2 = vbl) then Tbool
                                                | Var s1,_ -> if (s1 = vbl) then Tbool
                                                | _, Var s1 -> if (s1 = vbl) then Tbool 
                                                | _,_ -> lastTry g x vbl )
    | Tuple(x,y) -> (match x,y with
                      | Var s1,_ -> if (s1 = vbl) then Tint )
    | Project(x,y) -> (match x with
                      | Var s1,_ | _,Var s1 -> if (s1=vbl) then Tint)
    | FunctionCall(x,y) -> (match x,y with
                            | _, Var s1 -> let temp = retFun g x in (match temp with
                                            | Tfunc(a,b) -> if (s1=vbl) then a) )
      *)
    

let rec retFun g e = (match e with
                  | N x -> Tint
                  | B x -> Tbool
                  | Var s -> getTypeFromList g s
                  | Abs x | Negative x | Not x-> retFun g x 
                  | Add(x,y) | Sub(x,y) | Mult(x,y) | Div(x,y) | Rem(x,y) -> let temp1 = retFun g x in let temp2 = retFun g y in if (temp1 = temp2) && (temp1 = Tint) then Tint else Tunit
                  | Equals(x,y) | GreaterT(x,y) | GreaterTE(x,y) | LessT(x,y) | LessTE(x,y) -> let temp1 = retFun g x in let temp2 = retFun g y in if (temp1 = temp2) && (temp1 = Tint) then Tbool else Tunit
                  | Conjunction(x,y) | Disjunction(x,y) -> let temp1 = retFun g x in let temp2 = retFun g y in if (temp1 = temp2) && (temp1 = Tbool) then Tbool else Tunit
                  | InParen x -> retFun g x 
                  | IfThenElse(x,y,z) -> let ret1 = retFun g y in let ret2 = retFun g z in
                                          if (ret1=ret2) then ret1 else Tunit 
                  | Tuple(n,l) -> let rec listAdder l =  
                                                  (match l with
                                                  | [] -> []
                                                  | x::xs -> let temp = retFun g x in temp::listAdder xs)  
                                                in let temp2 = listAdder l in Ttuple(temp2) 
                  | Project(a,l) -> let temp = retFun g l in let i,n = a in (match temp with 
                                                                              | Ttuple x -> ithFromList x i
                                                                              | _ -> raise TypeError)
                  | FunctionAbstraction(s,l) -> let temp2 = shouldFun g l in let pred = shouldFun g (Var s) in Tfunc(pred,temp2)
                  | FunctionCall(a,b) -> let temp = retFun g a in let temp2 = retFun g b in (match temp,temp2 with 
                                                                                              | Tfunc(a,b),Tfunc(c,d) -> if (d=a) then Tfunc(c,b) else Tunit
                                                                                              | Tfunc(a,b),c -> if (c=a) then b else Tunit
                                                                                              | _ -> raise TypeError)
                                                                                              )
                  (* | Let(a,b) -> let newg = retGamma g a in retFun (g @ newg) b) *)


let rec retGamma g d = match d with
                  | Simple(s,e) -> let ret = retFun g e in let newEle = (s,ret) in [newEle]
                  | Sequence(dl) -> let rec seqCal g dl newL= (match dl with
                                                                | [] -> newL
                                                                | x::xs -> let temp = retGamma g x in seqCal (temp @ g) xs (temp @ newL)) 
                                                    in seqCal g dl []
                  | Parallel(dl) -> let rec parCal g dl = (match dl with
                                                                | [] -> []
                                                                | x::xs -> let temp = retGamma g x in temp @ (parCal g xs) )
                                                    in parCal g dl
                  | Local(d1,d2) -> let addedGamma = retGamma g d1 in retGamma (addedGamma @ g) d2

let rec hastype g e t = match e with 
                        | Let(a,b) -> let newg = retGamma g a in let ans = retFun (g @ newg ) b in if (ans = t) then true else false
                        | _ -> let temp3 = retFun g e  in if (temp3 = Tunit) then false else if (temp3 = t) then true else false


(* Function 'yields' takes a set of type assumptions G, a definition d and
another set of type assumptions G', and decides whether under the given
assumptions G, the definition d yields the type assumptions G' or not. *)

let rec yields g d g_dash = let newGamma = retGamma g d in let ans = chk2L newGamma g_dash in ans