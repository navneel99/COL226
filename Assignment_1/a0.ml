(* type declaration *)
type bigint = sign * int list and sign  = Neg | NonNeg;;

exception Divide_by_zero of string ;;
exception Not_an_int of string;;

(* reverse the list *)
let rec rev l = match l with
|  [] -> []
| x::xs -> (rev xs) @ [x];;

(* length of the list *)
let rec length l = match l with
    | [] -> 0
    | x::xs -> 1 + length xs;;
(* check if the signs are same *)
let sameSign s1 s2 = if (s1 = s2) then true else false;;

(* Remove the leading zeros *)
let rec remLeadZeros l = match l with
    |   [] -> []
    |   [x] -> [x];
    |   x::xs -> if x=0 then remLeadZeros xs else l;;

(* is the first number greater than the second ? *)
let bigger l1 l2 = let l1 = remLeadZeros l1 and l2 = remLeadZeros l2 in
    let chk =   if length l1 > length l2 then true 
                else if length l1 = length l2 then
                    let rec chk2 l1 l2 = match l1,l2 with
                        | [],[] -> false
                        |  x1::xs1,x2::xs2 -> if (x1>x2) then true else if (x1 = x2) then chk2 xs1 xs2 else false
                    in chk2 l1 l2
                else
                    false
    in chk;;

let zeroChecker a = let s1, l1 = a in
                        if (l1 = [0] || l1 = [] ) then (NonNeg,[0]) else a;;

(* Equal *)
let rec inteq l1 l2 = let l1 = remLeadZeros l1 and l2 = remLeadZeros l2 in
        let chk = if length l1 = length l2 then
                match l1, l2 with
                    | [],[] -> true
                    | x1::xs1,x2::xs2 -> if (x1 = x2) then  inteq xs1 xs2 else false
                else
                false
        in chk;;

let eq a1 a2 = let s1,l1 = a1 and s2,l2 = a2 in
            let a = sameSign s1 s2 and b = inteq l1 l2 in
                a && b;;
(* Greater Than *)

let gt a1 a2 =  let s1,l1 = a1 and s2,l2 = a2 in 
                let a =sameSign s1 s2 in
                if (a = true) then if (s1 = NonNeg) then bigger l1 l2 else let tL = bigger l1 l2 in not tL
                else if (s1 = NonNeg) then true else false;;
(* Less Than  *)
let lt a1 a2 =  let s1,l1 = a1 and s2,l2 = a2 in 
                let a =sameSign s1 s2 in
                if (a = true) then if (s1 = NonNeg) then bigger l2 l1 else  bigger l2 l1 
                else if (s1 = NonNeg) then false else true;;
(* greater than or equal to *)
let geq a1 a2 =  let a = gt a1 a2 and b = eq a1 a2 in
                match a,b with
                    |  false,false -> false 
                    |   _,_ -> true;;
(* less than or equal to *)
let leq a1 a2 =  let a = lt a1 a2 and b = eq a1 a2 in
                match a,b with
                    |  false,false -> false 
                    |   _,_ -> true;;

let rec addlist l1 l2 carry = 
    match l1,l2,carry with
        | [],[],x -> [x]
        | x1::xs1,[],x | [], x1::xs1,x -> let a = (x1 + x) mod 10 and b = (x1 + x)/10 in a :: addlist xs1 [] b
        | x1::xs1, x2::xs2,x -> let a = (x1 + x2 + x)mod 10  and b = (x1 + x2 +x)/10 in a :: addlist xs1 xs2 b;;  

let intadd l1 l2 =
                let tl1 = rev l1 and tl2 = rev l2 in 
                let nonRev = addlist tl1 tl2 0 in 
                let final = rev nonRev in remLeadZeros final;;
                

let subresult i1 i2 c = if (i1 -c)<i2 then 10 + i1 -c -i2 else i1-c-i2;;

let subcarry i1 i2 c = if (i1-c)<i2 then 1 else 0;;

(* i1-i2 with a carry overhead from the lesser digits *)
(*  sub list is from lsb to msb  and we assume that l1 >= l2*)
let rec sublist l1 l2 c = match l1,l2,c with
    |  [],[],_ -> []
    |  x::xs,[],c -> if(x-c < 0)then let a = subresult x 0 c and b = subcarry x 0 c in a :: sublist xs [] b else (x-c)::xs
    |  x1::xs1,x2::xs2,c-> let a = subresult x1 x2 c and b = subcarry x1 x2 c in a :: (sublist xs1 xs2 b);;

let intsub l1 l2 =  let tl1 = rev l1 and tl2 = rev l2 in 
                    let nonrev = sublist tl1 tl2 0 in 
                    let final = rev nonrev in remLeadZeros final;;


let add a1 a2 =  let s1,l1 = a1 and s2,l2 = a2 in 
                    let a = sameSign s1 s2 and b = bigger l1 l2 in 
                    let compute = match a,b with
                        |  true,_ -> s1 , intadd l1 l2
                        |  false,false -> s2 , intsub l2 l1
                        |  false,true -> s1 , intsub l1 l2
                    in zeroChecker compute;;

let sub a1 a2 =  let s1,l1 = a1 and s2,l2 = a2 in 
                    let a = sameSign s1 s2 and b = bigger l1 l2 in 
                    let compute = match a,b with
                        |  false,_ -> s1 , intadd l1 l2
                        |  true,false -> if s1 = NonNeg then Neg,intsub l2 l1 else NonNeg,intsub l2 l1 
                        |  true,true -> s1 , intsub l1 l2
                    in zeroChecker compute;;


let mulres i1 i2 c =  (i1*i2 + c) mod 10;;
let mulcarry i1 i2 c = (i1*i2 + c)/10;;
(* l1 is LSB to MSB *)
let rec sgldigmul i1 l1 c = match l1,c with
    |   [],c->if (c=0) then [] else [c]
    |   x::xs,c ->  let a = mulres i1 x c and b = mulcarry i1 x c in a :: sgldigmul i1 xs b;;

(* both are LSB to MSB *)
let rec mullist l1 l2 = match l1 with
    |   [] -> []
    |   x::xs -> addlist (sgldigmul x l2 0) (0::mullist xs l2) 0;; 

let intmul l1 l2 =  let a = rev l1 and b = rev l2 in 
                    let ans = mullist a b in 
                    let final = rev ans in remLeadZeros final;; 

let mult a1 a2 =  let s1,l1 = a1 and s2,l2 = a2 in
                    let a = sameSign s1 s2 in 
                    match a with
                        | true -> NonNeg,intmul l1 l2
                        | false -> Neg,intmul l1 l2;; 
(* division *)

let rec leastbig l2 acc l1 = match l2 with
    | [] -> acc
    | x::(xs) ->  if bigger acc l1 = true then acc else leastbig xs (acc @ [x]) l1;;

let rec remain l2 acc l1 = match l2 with
    |   [] -> l2
    |   x::xs -> if bigger acc l1 = true then x::xs else remain xs (acc @ [x] ) l1;;


let rec highMul big small current = if (bigger current big  = true) then intsub current small
                                        else if (inteq current big = true) then current
                                        else let newCurr = intadd current small in highMul big small newCurr;;   

let rec highQuo big small current itr = if (bigger current big  = true) then intsub itr [1]
                                        else if (inteq current big = true) then itr
                                        else let newCurr = intadd current small and newQuo = intadd itr [1] in highQuo big small newCurr newQuo;;   


let rec intQuo l2 l1 =  let a = leastbig l2 [] l1 and b = remain l2 [] l1 in
                        let c = highMul a l1 [] and d = highQuo a l1 [] [] in 
                        let e = intsub a c in
                            if b = [] then d else
                            d @ intQuo (e @ b) l1;;

let rec intRem l2 l1 =  let a = leastbig l2 [] l1 and b = remain l2 [] l1 in
                        let c = highMul a l1 [] in 
                        let e = intsub a c in
                            if b = [] then e else
                            intRem (e @ b) l1;;

let div a1 a2 = let s1,l1 = a1 and s2,l2 = a2 in
                let chk = inteq l2 [0] in if chk = true then raise (Divide_by_zero "Can't divide by zero. Sorry!") else
                let temp = if (sameSign s1 s2 = true) then NonNeg else Neg and temp2 = intQuo l1 l2 in
                (temp,temp2);;
                
                
let rem a1 a2 = let s1,l1 = a1 and s2,l2 = a2 in
                (* let temp = if (sameSign s1 s2 = true) then NonNeg else Neg and temp2 = intRem l1 l2 in *)
                let chk = inteq l2 [0] in if chk = true then raise (Divide_by_zero "Can't divide by zero. Sorry!") else                
                let temp = s1 and temp2 = intRem l1 l2 in
                (temp,temp2);;              


(* Unary Negation *)

let minus a1 = let s1,l1 = a1 in
            (if (s1 = Neg) then NonNeg else Neg),l1;;

(* Absolute Value *)
let abs a1 = let s1,l1 = a1 in (NonNeg,l1);;


(* Print Bigint as String *)
let print_num a1 =  let s1,l1 = a1 in
                    let l2 = List.map string_of_int l1 and s2 = if (s1 = Neg) then "-" else "" in
                    s2^List.fold_left (^) "" l2;;


let rec createList i =  if (i = 0) then [] else 
                            let q = i/10 and r = i mod 10 in
                            (createList q) @ [r];;

let mk_big i =  if (i = 0) then (NonNeg,[i]) else
                let a = if (i<0) then -i else i and b = if (i<0) then Neg else NonNeg in
                let li = createList a in (b,li);;

