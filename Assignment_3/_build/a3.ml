type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | ABS
  | TILDA
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | REM
  | CONJ
  | DISJ
  | EQ
  | GT
  | LT
  | LP
  | RP
  | IF
  | THEN
  | ELSE
  | FI
  | COMMA
  | PROJ
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "a3.mly"
    open A1
# 33 "a3.ml"
let yytransl_const = [|
  260 (* ABS *);
  261 (* TILDA *);
  262 (* NOT *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* REM *);
  268 (* CONJ *);
  269 (* DISJ *);
  270 (* EQ *);
  271 (* GT *);
  272 (* LT *);
  273 (* LP *);
  274 (* RP *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* FI *);
  279 (* COMMA *);
  280 (* PROJ *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\006\000\
\006\000\007\000\007\000\007\000\008\000\008\000\008\000\009\000\
\009\000\009\000\010\000\010\000\010\000\010\000\011\000\011\000\
\011\000\012\000\012\000\013\000\013\000\014\000\014\000\014\000\
\015\000\015\000\005\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\003\000\001\000\002\000\001\000\003\000\
\001\000\004\000\003\000\001\000\004\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\003\000\001\000\002\000\002\000\
\001\000\007\000\001\000\007\000\001\000\003\000\002\000\001\000\
\003\000\003\000\001\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\036\000\037\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\039\000\000\000\000\000\005\000\032\000\
\000\000\000\000\000\000\000\000\000\000\022\000\025\000\027\000\
\029\000\024\000\023\000\000\000\006\000\031\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\038\000\000\000\
\030\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\021\000\019\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\028\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\032\000"

let yysindex = "\037\000\
\051\255\000\000\000\000\000\000\000\000\123\255\123\255\013\255\
\005\255\051\255\002\255\000\000\005\000\033\255\000\000\000\000\
\034\255\046\255\049\255\053\255\063\255\000\000\000\000\000\000\
\000\000\000\000\000\000\051\255\000\000\000\000\021\255\064\255\
\045\255\068\255\051\255\000\000\051\255\123\255\075\255\099\255\
\123\255\123\255\123\255\123\255\123\255\019\255\000\000\051\255\
\000\000\051\255\058\255\033\255\000\000\046\255\123\255\049\255\
\123\255\053\255\063\255\063\255\000\000\000\000\000\000\023\255\
\000\000\012\255\083\255\049\255\053\255\051\255\067\255\018\255\
\025\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\149\000\000\000\000\000\
\143\000\119\000\083\000\047\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\155\000\000\000\131\000\000\000\095\000\
\000\000\059\000\018\000\035\000\000\000\000\000\000\000\069\255\
\000\000\000\000\000\000\107\000\071\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\249\255\051\000\053\000\080\000\000\000\055\000\221\255\
\216\255\025\000\006\000\000\000\000\000\023\000\043\000"

let yytablesize = 434
let yytable = "\058\000\
\018\000\031\000\033\000\056\000\036\000\003\000\004\000\005\000\
\006\000\007\000\008\000\026\000\027\000\003\000\004\000\005\000\
\069\000\017\000\034\000\068\000\046\000\009\000\030\000\010\000\
\035\000\003\000\004\000\005\000\011\000\028\000\035\000\035\000\
\070\000\035\000\016\000\035\000\047\000\001\000\047\000\074\000\
\064\000\009\000\066\000\048\000\037\000\048\000\015\000\038\000\
\061\000\062\000\063\000\003\000\004\000\005\000\006\000\007\000\
\008\000\035\000\014\000\041\000\042\000\039\000\072\000\040\000\
\050\000\059\000\060\000\009\000\051\000\010\000\013\000\043\000\
\044\000\045\000\011\000\003\000\004\000\005\000\006\000\007\000\
\067\000\049\000\012\000\071\000\073\000\052\000\033\000\029\000\
\055\000\053\000\065\000\009\000\054\000\010\000\011\000\075\000\
\000\000\000\000\011\000\003\000\004\000\005\000\006\000\007\000\
\000\000\000\000\010\000\000\000\000\000\000\000\000\000\000\000\
\057\000\000\000\000\000\009\000\000\000\010\000\009\000\000\000\
\000\000\000\000\011\000\003\000\004\000\005\000\006\000\007\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\000\000\010\000\007\000\000\000\
\000\000\000\000\011\000\000\000\003\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\018\000\000\000\000\000\000\000\018\000\018\000\018\000\018\000\
\018\000\035\000\018\000\000\000\018\000\018\000\018\000\018\000\
\017\000\017\000\000\000\000\000\000\000\017\000\017\000\017\000\
\017\000\017\000\000\000\017\000\000\000\017\000\017\000\017\000\
\017\000\016\000\016\000\000\000\000\000\000\000\016\000\016\000\
\016\000\016\000\016\000\000\000\016\000\000\000\016\000\016\000\
\016\000\016\000\015\000\015\000\015\000\015\000\015\000\000\000\
\015\000\000\000\015\000\015\000\015\000\015\000\014\000\014\000\
\014\000\014\000\014\000\000\000\014\000\000\000\014\000\014\000\
\014\000\014\000\013\000\013\000\013\000\013\000\013\000\000\000\
\013\000\000\000\013\000\013\000\013\000\013\000\012\000\012\000\
\012\000\000\000\012\000\000\000\012\000\000\000\012\000\012\000\
\012\000\012\000\011\000\011\000\011\000\000\000\011\000\000\000\
\011\000\000\000\011\000\011\000\011\000\011\000\010\000\010\000\
\010\000\000\000\010\000\000\000\010\000\000\000\010\000\010\000\
\010\000\010\000\009\000\009\000\009\000\000\000\000\000\000\000\
\009\000\000\000\009\000\009\000\009\000\009\000\008\000\008\000\
\008\000\000\000\000\000\000\000\008\000\000\000\008\000\008\000\
\008\000\008\000\007\000\007\000\000\000\000\000\000\000\000\000\
\007\000\003\000\007\000\007\000\007\000\007\000\003\000\002\000\
\003\000\003\000\003\000\003\000\002\000\000\000\002\000\002\000\
\002\000\002\000"

let yycheck = "\040\000\
\000\000\009\000\010\000\039\000\000\000\001\001\002\001\003\001\
\004\001\005\001\006\001\006\000\007\000\001\001\002\001\003\001\
\057\000\000\000\017\001\055\000\028\000\017\001\018\001\019\001\
\013\001\001\001\002\001\003\001\024\001\017\001\013\001\013\001\
\021\001\013\001\000\000\013\001\018\001\001\000\018\001\022\001\
\048\000\017\001\050\000\023\001\012\001\023\001\000\000\014\001\
\043\000\044\000\045\000\001\001\002\001\003\001\004\001\005\001\
\006\001\013\001\000\000\007\001\008\001\016\001\070\000\015\001\
\020\001\041\000\042\000\017\001\001\001\019\001\000\000\009\001\
\010\001\011\001\024\001\001\001\002\001\003\001\004\001\005\001\
\023\001\018\001\000\000\001\001\018\001\035\000\018\001\008\000\
\014\001\037\000\048\000\017\001\038\000\019\001\000\000\073\000\
\255\255\255\255\024\001\001\001\002\001\003\001\004\001\005\001\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\014\001\255\255\255\255\017\001\255\255\019\001\000\000\255\255\
\255\255\255\255\024\001\001\001\002\001\003\001\004\001\005\001\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\017\001\255\255\019\001\000\000\255\255\
\255\255\255\255\024\001\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\013\001\018\001\255\255\020\001\021\001\022\001\023\001\
\007\001\008\001\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\255\255\020\001\021\001\022\001\
\023\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\012\001\013\001\014\001\015\001\016\001\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\012\001\013\001\014\001\015\001\016\001\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\012\001\013\001\
\014\001\255\255\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\012\001\013\001\014\001\255\255\016\001\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\012\001\013\001\
\014\001\255\255\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\012\001\013\001\014\001\255\255\255\255\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\012\001\013\001\
\014\001\255\255\255\255\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\012\001\013\001\255\255\255\255\255\255\255\255\
\018\001\013\001\020\001\021\001\022\001\023\001\018\001\013\001\
\020\001\021\001\022\001\023\001\018\001\255\255\020\001\021\001\
\022\001\023\001"

let yynames_const = "\
  ABS\000\
  TILDA\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  REM\000\
  CONJ\000\
  DISJ\000\
  EQ\000\
  GT\000\
  LT\000\
  LP\000\
  RP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  COMMA\000\
  PROJ\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'and_expr) in
    Obj.repr(
# 26 "a3.mly"
                 ( _1 )
# 276 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_expr) in
    Obj.repr(
# 29 "a3.mly"
                          ( Disjunction(_1,_3) )
# 284 "a3.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_expr) in
    Obj.repr(
# 30 "a3.mly"
                          ( _1 )
# 291 "a3.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'not_expr) in
    Obj.repr(
# 33 "a3.mly"
                          ( Conjunction(_1,_3) )
# 299 "a3.ml"
               : 'or_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'not_expr) in
    Obj.repr(
# 34 "a3.mly"
                          ( _1 )
# 306 "a3.ml"
               : 'or_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 38 "a3.mly"
                          ( Not(_2) )
# 313 "a3.ml"
               : 'not_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eq_expr) in
    Obj.repr(
# 39 "a3.mly"
                        ( _1 )
# 320 "a3.ml"
               : 'not_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'eq_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lt_expr) in
    Obj.repr(
# 43 "a3.mly"
                         ( Equals(_1,_3) )
# 328 "a3.ml"
               : 'eq_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lt_expr) in
    Obj.repr(
# 44 "a3.mly"
                         ( _1 )
# 335 "a3.ml"
               : 'eq_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lt_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'gt_expr) in
    Obj.repr(
# 47 "a3.mly"
                          ( LessTE(_1,_4) )
# 343 "a3.ml"
               : 'lt_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lt_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'gt_expr) in
    Obj.repr(
# 48 "a3.mly"
                        ( LessT(_1,_3) )
# 351 "a3.ml"
               : 'lt_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'gt_expr) in
    Obj.repr(
# 49 "a3.mly"
                       ( _1 )
# 358 "a3.ml"
               : 'lt_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'gt_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'add_sub_expr) in
    Obj.repr(
# 53 "a3.mly"
                               ( GreaterTE(_1,_4) )
# 366 "a3.ml"
               : 'gt_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'gt_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_sub_expr) in
    Obj.repr(
# 54 "a3.mly"
                              ( GreaterT(_1,_3) )
# 374 "a3.ml"
               : 'gt_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_sub_expr) in
    Obj.repr(
# 55 "a3.mly"
                              ( _1 )
# 381 "a3.ml"
               : 'gt_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_sub_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rem_expr) in
    Obj.repr(
# 59 "a3.mly"
                                ( Sub(_1,_3) )
# 389 "a3.ml"
               : 'add_sub_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_sub_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rem_expr) in
    Obj.repr(
# 60 "a3.mly"
                                ( Add(_1,_3) )
# 397 "a3.ml"
               : 'add_sub_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rem_expr) in
    Obj.repr(
# 61 "a3.mly"
                           ( _1 )
# 404 "a3.ml"
               : 'add_sub_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rem_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expr) in
    Obj.repr(
# 65 "a3.mly"
                          ( Rem(_1,_3) )
# 412 "a3.ml"
               : 'rem_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rem_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expr) in
    Obj.repr(
# 66 "a3.mly"
                            ( Mult(_1,_3) )
# 420 "a3.ml"
               : 'rem_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rem_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expr) in
    Obj.repr(
# 67 "a3.mly"
                          ( Div(_1,_3) )
# 428 "a3.ml"
               : 'rem_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expr) in
    Obj.repr(
# 68 "a3.mly"
                         ( _1 )
# 435 "a3.ml"
               : 'rem_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expr) in
    Obj.repr(
# 82 "a3.mly"
                           ( Negative(_2))
# 442 "a3.ml"
               : 'abs_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'abs_expr) in
    Obj.repr(
# 83 "a3.mly"
                         ( Abs(_2) )
# 449 "a3.ml"
               : 'abs_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ifte_expr) in
    Obj.repr(
# 84 "a3.mly"
                           ( _1 )
# 456 "a3.ml"
               : 'abs_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'and_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'and_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'and_expr) in
    Obj.repr(
# 91 "a3.mly"
                                               ( IfThenElse(_2,_4,_6) )
# 465 "a3.ml"
               : 'ifte_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proj_expr) in
    Obj.repr(
# 92 "a3.mly"
                                      ( _1 )
# 472 "a3.ml"
               : 'ifte_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'tup_expr) in
    Obj.repr(
# 96 "a3.mly"
                                      ( Project((_3,_5),_7))
# 481 "a3.ml"
               : 'proj_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tup_expr) in
    Obj.repr(
# 97 "a3.mly"
             ( _1 )
# 488 "a3.ml"
               : 'proj_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rem_par) in
    Obj.repr(
# 101 "a3.mly"
                  ( let a,b = _2 in Tuple(a,b) )
# 495 "a3.ml"
               : 'tup_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "a3.mly"
             (Tuple(0,[]))
# 501 "a3.ml"
               : 'tup_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 103 "a3.mly"
             ( _1 )
# 508 "a3.ml"
               : 'tup_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 107 "a3.mly"
                           ( (2,[_1;_3]) )
# 516 "a3.ml"
               : 'rem_par))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rem_par) in
    Obj.repr(
# 108 "a3.mly"
                           (let x,y = _3 in (x+1,_1::y))
# 524 "a3.ml"
               : 'rem_par))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "a3.mly"
                           ( Var(_1) )
# 531 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 113 "a3.mly"
                           ( N(_1) )
# 538 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 114 "a3.mly"
                            ( B(_1) )
# 545 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'and_expr) in
    Obj.repr(
# 115 "a3.mly"
                                ( InParen(_2) )
# 552 "a3.ml"
               : 'constant))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : A1.exptree)
