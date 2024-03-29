type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | PLUS
  | CONJ
  | DISJ
  | IF
  | THEN
  | ELSE
  | FI
  | CMP
  | TIMES
  | BACKSLASH
  | DOT
  | LP
  | RP
  | SEMICOLON
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "a5gram.mly"
  open A5
# 26 "a5gram.ml"
let yytransl_const = [|
  260 (* PLUS *);
  261 (* CONJ *);
  262 (* DISJ *);
  263 (* IF *);
  264 (* THEN *);
  265 (* ELSE *);
  266 (* FI *);
  267 (* CMP *);
  268 (* TIMES *);
  269 (* BACKSLASH *);
  270 (* DOT *);
  271 (* LP *);
  272 (* RP *);
  273 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\004\000\
\005\000\005\000\006\000\006\000\007\000\007\000\008\000\008\000\
\009\000\009\000\010\000\010\000\010\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\003\000\001\000\003\000\003\000\001\000\
\002\000\001\000\007\000\001\000\004\000\001\000\004\000\001\000\
\003\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\021\000\020\000\019\000\000\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\008\000\010\000\012\000\
\000\000\016\000\018\000\000\000\009\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\007\000\006\000\000\000\000\000\015\000\013\000\
\000\000\000\000\011\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000"

let yysindex = "\015\000\
\019\255\000\000\000\000\000\000\000\000\019\255\037\255\010\255\
\019\255\000\000\004\000\006\255\002\255\000\000\000\000\000\000\
\012\255\000\000\000\000\038\255\000\000\029\255\254\254\019\255\
\000\000\019\255\019\255\019\255\019\255\019\255\010\255\000\000\
\006\255\002\255\000\000\000\000\001\255\027\255\000\000\000\000\
\019\255\005\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\010\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\037\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\009\000\023\000\020\000\034\000\000\000\000\000\
\254\255\000\000"

let yytablesize = 309
let yytable = "\020\000\
\014\000\024\000\023\000\025\000\024\000\022\000\027\000\028\000\
\024\000\005\000\003\000\004\000\005\000\032\000\043\000\001\000\
\040\000\026\000\004\000\003\000\004\000\005\000\037\000\038\000\
\009\000\006\000\029\000\003\000\039\000\007\000\024\000\008\000\
\033\000\009\000\042\000\041\000\002\000\003\000\004\000\005\000\
\021\000\024\000\031\000\006\000\000\000\030\000\035\000\036\000\
\034\000\008\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\014\000\014\000\024\000\
\014\000\014\000\014\000\000\000\014\000\005\000\000\000\000\000\
\014\000\005\000\005\000\005\000\000\000\005\000\004\000\000\000\
\000\000\005\000\004\000\004\000\004\000\000\000\004\000\003\000\
\000\000\000\000\004\000\003\000\003\000\003\000\000\000\000\000\
\002\000\000\000\000\000\003\000\002\000\002\000\002\000\000\000\
\000\000\000\000\000\000\000\000\002\000"

let yycheck = "\006\000\
\000\000\004\001\009\000\000\000\004\001\008\000\005\001\006\001\
\004\001\000\000\001\001\002\001\003\001\016\001\010\001\001\000\
\016\001\012\001\000\000\001\001\002\001\003\001\029\000\030\000\
\015\001\007\001\015\001\000\000\031\000\011\001\004\001\013\001\
\024\000\015\001\041\000\009\001\000\000\001\001\002\001\003\001\
\007\000\004\001\014\001\007\001\255\255\008\001\027\000\028\000\
\026\000\013\001\255\255\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\006\001\004\001\
\008\001\009\001\010\001\255\255\012\001\004\001\255\255\255\255\
\016\001\008\001\009\001\010\001\255\255\012\001\004\001\255\255\
\255\255\016\001\008\001\009\001\010\001\255\255\012\001\004\001\
\255\255\255\255\016\001\008\001\009\001\010\001\255\255\255\255\
\004\001\255\255\255\255\016\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\255\255\016\001"

let yynames_const = "\
  PLUS\000\
  CONJ\000\
  DISJ\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  CMP\000\
  TIMES\000\
  BACKSLASH\000\
  DOT\000\
  LP\000\
  RP\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'plus_parser) in
    Obj.repr(
# 16 "a5gram.mly"
                    ( _1 )
# 207 "a5gram.ml"
               : A5.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plus_parser) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_parser) in
    Obj.repr(
# 19 "a5gram.mly"
                                 (Plus(_1,_3))
# 215 "a5gram.ml"
               : 'plus_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_parser) in
    Obj.repr(
# 20 "a5gram.mly"
                ( _1 )
# 222 "a5gram.ml"
               : 'plus_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_parser) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_parser) in
    Obj.repr(
# 24 "a5gram.mly"
                                  ( Mult(_1,_3) )
# 230 "a5gram.ml"
               : 'mult_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_parser) in
    Obj.repr(
# 25 "a5gram.mly"
                ( _1 )
# 237 "a5gram.ml"
               : 'mult_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_parser) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmp_parser) in
    Obj.repr(
# 29 "a5gram.mly"
                                ( Or(_1,_3) )
# 245 "a5gram.ml"
               : 'bool_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_parser) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmp_parser) in
    Obj.repr(
# 30 "a5gram.mly"
                                ( And(_1,_3) )
# 253 "a5gram.ml"
               : 'bool_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmp_parser) in
    Obj.repr(
# 31 "a5gram.mly"
               ( _1 )
# 260 "a5gram.ml"
               : 'bool_parser))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ifte_parser) in
    Obj.repr(
# 35 "a5gram.mly"
                    (Cmp _2 )
# 267 "a5gram.ml"
               : 'cmp_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ifte_parser) in
    Obj.repr(
# 36 "a5gram.mly"
                ( _1 )
# 274 "a5gram.ml"
               : 'cmp_parser))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'plus_parser) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'plus_parser) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'plus_parser) in
    Obj.repr(
# 39 "a5gram.mly"
                                                         (If_Then_Else(_2,_4,_6))
# 283 "a5gram.ml"
               : 'ifte_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'funCall_parser) in
    Obj.repr(
# 40 "a5gram.mly"
                   ( _1 )
# 290 "a5gram.ml"
               : 'ifte_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'funAbs_parser) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'plus_parser) in
    Obj.repr(
# 43 "a5gram.mly"
                                    (App(_1,_3))
# 298 "a5gram.ml"
               : 'funCall_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'funAbs_parser) in
    Obj.repr(
# 44 "a5gram.mly"
                  ( _1 )
# 305 "a5gram.ml"
               : 'funCall_parser))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'paren_parser) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'paren_parser) in
    Obj.repr(
# 47 "a5gram.mly"
                                            (Lambda(_2,_4))
# 313 "a5gram.ml"
               : 'funAbs_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'paren_parser) in
    Obj.repr(
# 48 "a5gram.mly"
                 (_1 )
# 320 "a5gram.ml"
               : 'funAbs_parser))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'plus_parser) in
    Obj.repr(
# 51 "a5gram.mly"
                      (_2)
# 327 "a5gram.ml"
               : 'paren_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const_parser) in
    Obj.repr(
# 52 "a5gram.mly"
                (_1)
# 334 "a5gram.ml"
               : 'paren_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "a5gram.mly"
       (V(_1) )
# 341 "a5gram.ml"
               : 'const_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 56 "a5gram.mly"
         (Bool(_1))
# 348 "a5gram.ml"
               : 'const_parser))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 57 "a5gram.mly"
        (Integer(_1))
# 355 "a5gram.ml"
               : 'const_parser))
(* Entry a5_parser *)
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
let a5_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : A5.expr)
