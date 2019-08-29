exception TypeError of string
type arg = ArgI of string * int | ArgS of string * string | Procedure of string 
  | LocalV of (arg) list | AllV of (arg) list | Frame of arg * (arg * arg) * arg * arg * arg
  | FrameList of arg list;;

(* type frame = Frame of string * (arg * arg) * int list ;; *)
val getArgs : string -> string list 

val saveStaticLink : arg -> arg 
val accessProc : arg -> string list 
val contains : 'a list -> 'a -> bool 
val getValue : arg list -> string -> int 
val argChanger : arg list -> arg -> arg 
val searchStackFrame : arg list -> arg -> arg 
val varToIntChange : arg list -> arg -> arg 
val getLocalV : string -> string list 
val getArgs : string -> string list 
val initLocalV : arg -> arg 
val remOldEle : arg list -> string -> arg list 
val updateList : arg list -> arg list -> arg list 
val getAllV : arg list -> arg -> arg 
val initializeFrame : arg list -> arg -> arg 
val addToFrame : arg list -> arg -> arg list 
val assignValue : arg list -> arg -> arg list 
val contains2 : arg list -> string -> bool 
val updateList2 : arg list -> arg list -> arg list 
val return : arg list -> arg list 
val returnLocalV : arg list -> arg list
val returnAllV : arg list -> arg list

