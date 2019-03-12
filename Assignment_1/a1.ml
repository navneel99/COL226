open A0 (* Should be in same folder*)

type  exptree =  N of int 
    | Plus of exptree *  exptree 
    | Minus of exptree *  exptree 
    | Mult of exptree *  exptree 
    | Div of exptree *  exptree 
    | Rem of exptree *  exptree 
    | Nega of  exptree 
    | Abs of  exptree ;;

type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS;;


let rec eval t = match t with 
    |   N x -> x
    |   Plus(x,y) ->  (eval x) + (eval y)
    |   Minus(x,y) ->  (eval x) - (eval y)
    |   Mult(x,y) ->  (eval x) * (eval y)
    |   Div(x,y) ->  (eval x)/(eval y)
    |   Rem(x,y) ->  (eval x)mod (eval y)
    |   Nega x -> -(eval x)
    |   Abs x ->  Pervasives.abs(eval x);;

exception Not_implemented;;

let sinopuse a b = match a with
    |   UNARYMINUS -> minus b
    |   ABS -> abs b;;

let twoopuse a b c = match a with
    |   PLUS -> add b c
    |   MINUS -> sub b c
    |   TIMES -> mult b c
    |   DIV -> div b c
    |   REM -> rem b c;;

let rec stackmc bl ol = match ol with
    |   [] -> List.hd(bl)
    |   x::xs -> match x with 
                |   CONST y -> stackmc (y :: bl) xs
                |   UNARYMINUS | ABS -> let fir_ele = List.hd(bl) and rem_list = List.tl(bl) in
                                        let ans = sinopuse x fir_ele in 
                                        stackmc (ans :: rem_list) xs
                |   PLUS | MINUS | TIMES | DIV | REM -> match bl with h::m::t -> let ans = twoopuse x h m in
                                                                                 stackmc (ans :: t) xs;;  
let rec compile t = match t with
    |   N x -> let y = mk_big(x) in [CONST y]
    |   Plus(x,y) -> (compile y) @ (compile x) @ [PLUS]
    |   Minus(x,y) ->(compile y) @ (compile x) @ [MINUS]
    |   Mult(x,y) -> (compile y) @ (compile x) @ [TIMES]
    |   Div(x,y) -> (compile y) @ (compile x) @ [DIV]
    |   Rem(x,y) -> (compile y) @ (compile x) @ [REM]
    |   Nega x -> (compile x) @ [UNARYMINUS]
    |   Abs(x) -> (compile x) @ [ABS];;


