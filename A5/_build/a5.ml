exception TypeError
exception NotFoundError
exception LessElementsError

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
and closure = Clos of expr * table
;;

type opclosure = (string * opcode list * table);;

type answer = Aint of int | Abool of bool| Avcl of opclosure;;

let rec getValueFromList a l = (match l with 
  | [] -> raise NotFoundError
  | x::xs -> let i,j = x in if (i = a) then j else getValueFromList a xs
  )

let rec opcodeLGenerator l = 
  (match l with
  | APP::tl -> []
  | x::tl ->  x::(opcodeLGenerator tl)
  | _ -> raise NotFoundError)

let rec remLoutput l =
  (match l with
    | APP::tl -> tl
    | x::tl -> remLoutput tl
    | _ -> raise NotFoundError)


let rec krivineMC c s = (match c with
  | Clos(x,y) -> (match x with
    | Integer a -> Clos(Integer a,y) 
    | Bool b -> Clos(Bool b,y)
    | V m -> let newc = getValueFromList (V m) y in krivineMC newc s
    | Cmp a -> let ans = (krivineMC(Clos(a,y)) s) in (match ans with
      | Clos(q,w) -> if (q >= Integer 0) then Clos(Bool true,w) else Clos(Bool false,w)
      | _ -> raise TypeError)
    | Plus(a,b) -> let Clos(exp1,tab1) = krivineMC (Clos(a,y)) s in let Clos(exp2,tab2) = krivineMC (Clos(b,y)) s in (match exp1,exp2 with
      | Integer e1, Integer e2 -> Clos(Integer(e1+e2),tab1 @ tab2)
      | _,_ -> raise TypeError)
    | Mult(a,b) -> let Clos(exp1,tab1) = krivineMC (Clos(a,y)) s in let Clos(exp2,tab2) = krivineMC (Clos(b,y)) s in (match exp1,exp2 with
      | Integer e1, Integer e2 -> Clos(Integer(e1*e2),tab1 @ tab2)
      | _,_ -> raise TypeError)
    | And(a,b) -> let Clos(exp1,tab1) = krivineMC (Clos(a,y)) s in let Clos(exp2,tab2) = krivineMC (Clos(b,y)) s in (match exp1,exp2 with
      | Bool e1, Bool e2 -> Clos(Bool(e1 && e2),tab1 @ tab2)
      | _,_ -> raise TypeError)
    | Or(a,b) -> let Clos(exp1,tab1) = krivineMC (Clos(a,y)) s in let Clos(exp2,tab2) = krivineMC (Clos(b,y)) s in (match exp1,exp2 with
      | Bool e1, Bool e2 -> Clos(Bool(e1 || e2),tab1 @ tab2)
      | _,_ -> raise TypeError)
    | Lambda(a,b) -> let hd::tl = s in krivineMC (Clos(b,((a,hd)::y))) tl
    | App(a,b) -> let ansClos = krivineMC (Clos(b,y)) s in krivineMC (Clos(a,y)) ((ansClos)::s)
    | If_Then_Else(a,b,c) -> let Clos(exp1,tab1) = krivineMC (Clos(a,y)) s in if (exp1 = Bool true) 
                              then  krivineMC (Clos(b,y)) s 
                              else  krivineMC (Clos(c,y)) s 
    )

  | _ -> raise TypeError)

let rec compile c t = match c with
  | Integer a -> [INT a]
  | Bool b -> [BOOL b]
  | Plus(a,b) -> let ans1 = compile a t in let ans2 = compile b t in ans2 @ ans1 @ [PLUS]
  | Mult(a,b) -> let ans1 = compile a t in let ans2 = compile b t in ans2 @ ans1 @ [MULT]
  | And(a,b) -> let ans1 = compile a t in let ans2 = compile b t in ans2 @ ans1 @ [AND]
  | Or(a,b) -> let ans1 = compile a t in let ans2 = compile b t in ans2 @ ans1 @ [OR]
  | Cmp a -> let ans1 = compile a t in ans1 @ [CMP]
  | V s -> [VID s]
  | App(a,b) -> let ans1 = compile a t in let ans2 = compile b t in ans1 @ ans2 @ [APP] (* Careful *)
  | If_Then_Else(a,b,c) -> let ans1 = compile a t in let ans2 = compile b t in let ans3 = compile c t in ans1 @ [COND(ans2,ans3)]
  | Lambda(a,b) -> let ans1 = compile a t in let ans2 = compile b t in [CLOS(ans1,ans2@[RET])]

let rec environmentSearch  s e = match e with
  | [] -> raise NotFoundError
  | x::xs -> let ex,cl = x in if (V s) = ex then cl else environmentSearch s xs

let envAddition s v e = let newE = (V s),Clos(v,[]) in newE::e 

let convertAnswerToExpr a = match a with
  | Aint b -> Integer b
  | Abool b -> Bool b
  | _ -> raise TypeError (* Careful *)


let rec secdMC s e c d = match c with
  | [] -> List.hd s
  | (INT x)::xs -> secdMC ((Aint x)::s) e xs d
  | (BOOL x)::xs -> secdMC ((Abool x)::s) e xs d
  | (VID m)::xs -> let (retClos) = environmentSearch m e in 
                   let Clos(exp,tab) = retClos in
                   let compiledAns = compile exp e in
                   let secdAns = secdMC [] (e) compiledAns [] in
                   secdMC (secdAns::s) e xs d
  | (CMP)::xs -> if (List.hd s >= Aint 0) then secdMC (Abool true::List.tl s) e xs d else secdMC (Abool false::List.tl s) e xs d
  | (PLUS)::xs -> (match s with
    | a::(b::left) -> let Aint(a1) = a in let Aint(a2) = b in secdMC (Aint(a1+a2)::left) e xs d
    | _ -> raise LessElementsError)
  | (MULT)::xs -> (match s with
    | a::(b::left) -> let Aint(a1) = a in let Aint(a2) = b in secdMC (Aint(a1*a2)::left) e xs d
    | _ -> raise LessElementsError)
  | (AND)::xs -> (match s with
    | a::(b::left) -> let Abool(a1) = a in let Abool(a2) = b in secdMC (Abool(a1 && a2)::left) e xs d
    | _ -> raise LessElementsError)
  | (OR)::xs -> (match s with
    | a::(b::left) -> let Abool(a1) = a in let Abool(a2) = b in secdMC (Abool(a1 || a2)::left) e xs d
    | _ -> raise LessElementsError)
  | (COND(a,b))::xs -> (match s with
    | x::left -> if (x = Abool true) then let ans = secdMC left e a d in secdMC (ans::left) e xs d else let ans = secdMC left e b d in secdMC (ans::left) e xs d 
    | _ -> raise LessElementsError)
  | (CLOS(a,b))::xs -> (match a with
                        | [VID m] -> let currOpClos = Avcl(m,b,e) in secdMC (currOpClos :: s) e  xs d
                        | _ -> raise TypeError)
  | (APP):: xs -> (match s with
    | a::b::tl -> let varAns = a in
                  (match b with
                    | Avcl(w,x,y) -> let newE = envAddition w (convertAnswerToExpr varAns) y in 
                                     secdMC [] newE x ((tl,e,xs)::d)  
                    | _ -> raise TypeError)
    | _ -> raise LessElementsError)
  | (RET)::xs -> let oldS, oldE, oldC = List.hd d in
                 let funRet = List.hd s in
                 secdMC (funRet::oldS) oldE oldC (List.tl d)
  | (COND(a,b))::xs -> (match s with
                        | Abool true :: tl -> secdMC tl e (a@xs) d 
                        | Abool false:: tl -> secdMC tl e (b@xs) d
                        | _ -> raise TypeError)
                 