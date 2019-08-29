type token =
  | ID of (string)
  | INT of (int)
  | FNAME of (string)
  | LP
  | RP
  | COMMA
  | COLON
  | EQ
  | RET
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "a6gram.mly"
  open A6
# 18 "a6gram.ml"
let yytransl_const = [|
  260 (* LP *);
  261 (* RP *);
  262 (* COMMA *);
  263 (* COLON *);
  264 (* EQ *);
  265 (* RET *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* INT *);
  259 (* FNAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\004\000\005\000\005\000\
\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\006\000\001\000\004\000\001\000\001\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\010\000\000\000\007\000\011\000\000\000\
\008\000\004\000\006\000\000\000\000\000\001\000\000\000\009\000\
\000\000\005\000\000\000\000\000\003\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\011\000"

let yysindex = "\003\000\
\255\254\000\000\003\255\000\000\007\255\000\000\000\000\012\000\
\000\000\000\000\000\000\006\255\005\255\000\000\005\255\000\000\
\009\255\000\000\005\255\008\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\016\000\000\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\246\255\000\000\000\000"

let yytablesize = 17
let yytable = "\003\000\
\004\000\005\000\017\000\001\000\018\000\016\000\004\000\006\000\
\020\000\012\000\013\000\014\000\021\000\015\000\019\000\009\000\
\002\000"

let yycheck = "\001\001\
\002\001\003\001\013\000\001\000\015\000\001\001\002\001\009\001\
\019\000\007\001\004\001\000\000\005\001\008\001\006\001\000\000\
\000\000"

let yynames_const = "\
  LP\000\
  RP\000\
  COMMA\000\
  COLON\000\
  EQ\000\
  RET\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  FNAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'funCall_parser) in
    Obj.repr(
# 16 "a6gram.mly"
                       ( _1 )
# 98 "a6gram.ml"
               : A6.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 20 "a6gram.mly"
          ( Frame(Procedure(_1),(ArgI ("null",0),ArgI ("null",0)),LocalV [],AllV [], FrameList []))
# 105 "a6gram.ml"
               : 'funCall_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'const_parser) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'const_parser) in
    Obj.repr(
# 21 "a6gram.mly"
                                                ( let l = A6.getArgs _1 in ( match _3,_5 with
                                                                              | ArgI (x1,x2), ArgI (y1,y2) -> Frame(Procedure(_1),(ArgI (List.nth l 0,x2),ArgI(List.nth l 1, y2)) ,LocalV[],AllV[],FrameList[]) 
                                                                              | ArgI (x1,x2), ArgS (y1,y2) -> Frame(Procedure(_1),(ArgI (List.nth l 0,x2),ArgS(List.nth l 1, y2)) ,LocalV[],AllV[],FrameList[])
                                                                              | ArgS (x1,x2), ArgI (y1,y2) -> Frame(Procedure(_1),(ArgS (List.nth l 0,x2),ArgI(List.nth l 1, y2)) ,LocalV[],AllV[],FrameList[])
                                                                              | ArgS (x1,x2), ArgS (y1,y2) -> Frame(Procedure(_1),(ArgS (List.nth l 0,x2),ArgS(List.nth l 1, y2)) ,LocalV[],AllV[],FrameList[])
                                                                            ) )
# 119 "a6gram.ml"
               : 'funCall_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assign_parser) in
    Obj.repr(
# 27 "a6gram.mly"
                  ( _1 )
# 126 "a6gram.ml"
               : 'funCall_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'const_parser) in
    Obj.repr(
# 31 "a6gram.mly"
                             (( match _4 with
                                          |  ArgI (y1,y2) -> ArgI(_1,y2) 
                                          |  ArgS (y1,y2) -> ArgS(_1,y2) ))
# 136 "a6gram.ml"
               : 'assign_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ret_parser) in
    Obj.repr(
# 34 "a6gram.mly"
               ( _1 )
# 143 "a6gram.ml"
               : 'assign_parser))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "a6gram.mly"
        ( ArgI("null",0) )
# 149 "a6gram.ml"
               : 'ret_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const_parser) in
    Obj.repr(
# 38 "a6gram.mly"
                 ( _1 )
# 156 "a6gram.ml"
               : 'ret_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "a6gram.mly"
        ( ArgS ("dummy",_1) )
# 163 "a6gram.ml"
               : 'const_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "a6gram.mly"
        ( ArgI ("dummy",_1) )
# 170 "a6gram.ml"
               : 'const_parser))
(* Entry a6_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let a6_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : A6.arg)
