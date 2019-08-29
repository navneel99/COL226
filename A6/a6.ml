  exception TypeError of string
  type arg = ArgI of string * int | ArgS of string * string | Procedure of string 
    | LocalV of (arg) list | AllV of (arg) list | Frame of arg * (arg * arg) * arg * arg * arg
    | FrameList of arg list;;

  (* Frame has procedure,argument,localVariables and static link *)

  (* type frame = Frame of arg * (arg * arg) * int list ;; *)

  let saveStaticLink f = let Frame(s,a,l,al,sl) = f in 
  ( let staticUpdate s =
    match s with
      | Procedure p -> (match p with
        | "Main" -> FrameList []
        | "P" -> FrameList [Procedure "Main"]
        | "Q" -> FrameList [Procedure "Main"]
        | "R" -> FrameList [Procedure "P";Procedure "Main"]
        | "S" -> FrameList [Procedure "P";Procedure "Main"]
        | "T" -> FrameList [Procedure "Q";Procedure "Main"]
        | "U" -> FrameList [Procedure "Q";Procedure "Main"]
        | "V" -> FrameList [Procedure "R";Procedure "P";Procedure "Main"]
        | "W" -> FrameList [Procedure "T";Procedure "Q";Procedure "Main"])
      | _ -> raise (TypeError "Wrong Type of Frame Name given.")
    in Frame(s,a,l,al,(staticUpdate s))
      )

  let accessProc f = let Frame(s,a,l,al,sl) = f in (match s with
    | Procedure p -> (match p with
      | "Main" -> ["P";"Q"]
      | "P" -> ["P";"R";"S";"Q";]
      | "Q" -> ["P";"Q";"T";"U"]
      | "R" -> ["R";"V";"S";"P";"Q"]
      | "S" -> ["S";"P";"R";"Q";"V"]
      | "T" -> ["T";"W";"Q";"P";"U"]
      | "U" -> ["U";"T";"Q";"P"]
      | "V" -> ["V";"R";"S";"P";"Q"]
      | "W" -> ["W";"T";"Q";"P";"U"]
      | _ -> raise (TypeError "Frame doesn't match with then specified in the assignment."))
    | _ -> raise (TypeError "Frame is wrong."))

  let rec contains l1 s = match l1 with
    | [] -> false
    | x::xs -> if (x = s) then true else contains xs s


  (* l is a list ArgI string, int *)
  let rec getValue l s = (match l with
    | [] -> raise (TypeError "You used a variable that is not present.")
    | x::xs -> let ArgI (y,z) = x in if (s = y) then z else getValue xs s )

  let argChanger parAlv a = match a with
    | ArgS (x,y) -> let ans1 = getValue parAlv y  in ArgI(x,ans1)
    | _ -> a

  let rec searchStackFrame sf p = (match sf with
    | [] -> raise (TypeError "Something is wrong with your stack frame. Couldn't find your parent.")
    | x::xs -> let Frame(s1,a1,l1,al1,sl1) = x in if (p = s1) then x else searchStackFrame xs p)


  (* This function takes arguments that may be of form ArgS and replaces each with ArgI *)

  let varToIntChange sf f = let Frame(Procedure(s),a,l,al, FrameList(sl)) = f in 
      if (s = "Main") then f else
      let arg1,arg2 = a in 
      let parentProc = List.hd sl in
      let parentFrame = searchStackFrame sf parentProc in let Frame(sp,ap,LocalV lp,AllV alp, FrameList(slp)) = parentFrame in
      let modarg1 = argChanger alp arg1 in let modarg2 = argChanger alp arg2 in
      Frame(Procedure(s),(modarg1,modarg2),l,al,FrameList(sl))
      
  let getLocalV s = (match s with
    | "Main" -> ["a";"b";"c"]
    | "P" -> ["z";"a"]
    | "Q" -> ["x";"b"]
    | "R" -> ["j";"b"]
    | "S" -> ["m";"n"]
    | "T" -> ["i";"f"]
    | "U" -> ["p";"g"]
    | "V" -> ["c"]
    | "W" -> ["j";"h"]
    | _ -> raise (TypeError "Frame doesn't match with then specified in the assignment."))

  let getArgs s = (match s with
  | "Main" -> []
  | "P" -> ["x";"y"]
  | "Q" -> ["z";"w"]
  | "R" -> ["w";"i"]
  | "S" -> ["c";"k"]
  | "T" -> ["a";"y"]
  | "U" -> ["c";"z"]
  | "V" -> ["m";"n"]
  | "W" -> ["m";"p"]
  | _ -> raise (TypeError "Frame doesn't match with then specified in the assignment.") )

  let initLocalV  f = let Frame(Procedure(s),a,l,al,FrameList sl) = f in let varList = getLocalV s  in
      (* let Frame(Procedure(p),ap,lp,alp,FrameList slp) = searchStackFrame sf (List.hd sl) in *)
      let rec lMaker n a = match n with
        | 0 -> []
        | n ->  (ArgI (List.nth varList (n-1) , a) ) :: lMaker (n-1) a
      in let localVList = lMaker (List.length varList) 0 in
        Frame(Procedure(s),a,LocalV localVList,al,FrameList sl)

  let rec remOldEle l s = match l with
    | [] -> []
    | x::xs -> let ArgI(a,b) = x in if (a = s) then remOldEle xs s else x::remOldEle xs s


  (* This function adds 2 list, if theree is repeat then take the 2nd list's value *)

  let rec updateList l1 l2 =  match l2 with
    | [] -> l1
    | x::xs -> let ArgI(firEle, secEle) = x in let newL = remOldEle l1 firEle in x::updateList newL xs

  (* logic to be changed *)
  let getAllV sf f = let Frame(Procedure(p),a,LocalV l,AllV al,FrameList(sl)) = f in 
    if (p = "Main") then  Frame(Procedure(p),a,LocalV l, AllV l, FrameList(sl)) else
    let parentFrame = searchStackFrame sf (List.hd sl) in let Frame(Procedure(p1),a1,LocalV l1,AllV al1,FrameList(sl1)) = parentFrame in
    let t1,t2 = a in let argList = [t1;t2] in
    let compose = updateList al1 l in let compose = updateList compose argList in Frame(Procedure(p),a,LocalV l,AllV compose,FrameList(sl))
  (* let argParser  *)

  let initializeFrame sf f =  let newF = saveStaticLink f in  (* Static link created *)
                              let newF = initLocalV newF in  (* Local Variables initialized *)
                              let newF = varToIntChange sf newF in (* Arguments initialized *)
                              getAllV sf newF (* Add the all variables *) 
                              
  let addToFrame s f = let newf = initializeFrame s f in if (List.length s = 0) then newf::s else let hd = List.hd s in let avail = accessProc hd in 
    let Frame(Procedure(nm),a,l,al,sl) = newf  in let chk = contains avail nm in if (chk = true) then newf::s else raise (TypeError "Frame can't be added with the current head.");;

(* pair is of form ArgI or ArgS *)
  let assignValue sf pair = let Frame(Procedure(p),a,LocalV l,AllV al,sl)= List.hd sf in let tail = List.tl sf in 
    (match pair with
      | ArgI(x,y) ->  let newLocalL = remOldEle l x in 
                      let newLocalL = pair::newLocalL in
                      let newAllL = remOldEle al x in
                      let newAllL = pair :: newAllL in
                      let t1,t2 = a in let newArgL = [t1;t2] in
                      let newArgL = remOldEle newArgL x in
                      let newArgL = pair :: newArgL in
                      let newa = (List.nth newArgL 0),(List.nth newArgL 1) in
                      Frame(Procedure(p),newa,LocalV newLocalL,AllV newAllL,sl)::tail

      | ArgS(x,y) ->  let ans = getValue al y in
                      let newLocalL = remOldEle l x in 
                      let newLocalL = ArgI(x,ans)::newLocalL in
                      let newAllL = remOldEle al x in
                      let newAllL = ArgI(x,ans) :: newAllL in 
                      let t1,t2 = a in let newArgL = [t1;t2] in
                      let newArgL = remOldEle newArgL x in
                      let newArgL = ArgI(x,ans) :: newArgL in
                      let newa = (List.nth newArgL 0),(List.nth newArgL 1) in
                      Frame(Procedure(p),newa,LocalV newLocalL,AllV newAllL,sl)::tail
                      )
                      (* This function will update l2 with values from l1  *)
  let rec contains2 l s = match l with
    | [] -> false
    | x::xs -> let ArgI(a,b) = x in if (s = a) then true else contains2 xs s

  let rec updateList2 l1 l2 = match l1 with 
    | [] -> l2
    | x::xs -> let ArgI(a,b) = x in let newl = remOldEle l2 a in let chk = contains2 l2 a in if (chk = true) then x:: updateList2 xs newl else updateList2 xs l2 

  let return sf = let Frame(Procedure(p),a,LocalV l,AllV al,sl)= List.hd sf in let tail = List.tl sf in
    let Frame(Procedure(p1),a1,LocalV l1,AllV al1,sl1)= List.hd tail in let newtail = List.tl tail in
    let newAlV = updateList2 al al1 in
    let newLoV = updateList2 al l1 in
    let t1,t2 = a in let newArgL = [t1;t2] in
    let newArV = updateList2 al newArgL in
    let newa = (List.nth newArV 0),(List.nth newArV 1) in
    Frame(Procedure(p1),newa,LocalV newLoV,AllV newAlV,sl1):: newtail

  let returnLocalV sf = let Frame(Procedure(p),a,LocalV l,AllV al,sl)= List.hd sf in l
  let returnAllV sf = let Frame(Procedure(p),a,LocalV l,AllV al,sl)= List.hd sf in al


  let main = Frame(Procedure "Main",(ArgI ("a",1),ArgI ("b",2)),LocalV [],AllV [],FrameList [])
  let p = Frame(Procedure "P",(ArgI ("w",1),ArgI ("x",2)),LocalV [],AllV [],FrameList [])
  let q = Frame(Procedure "Q",(ArgI ("w",1),ArgI ("i",2)),LocalV [],AllV [],FrameList [])
  let r = Frame(Procedure "R",(ArgI ("w",1),ArgI ("i",2)),LocalV [],AllV [],FrameList [])

