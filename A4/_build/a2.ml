# 1 "a2.mll"
 
  open A3
  exception Not_implemented
  exception InvalidToken of char;;

let remPlus a = let b = (String.length a) in
             let c = a.[0] in 
if (c = '+') then (String.sub a 1 (b-1)) else a;;

# 12 "a2.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\219\255\079\000\002\000\001\000\224\255\225\255\000\000\
    \231\255\001\000\007\000\000\000\009\000\236\255\237\255\238\255\
    \011\000\001\000\009\000\242\255\243\255\244\255\010\000\036\000\
    \247\255\248\255\249\255\000\000\163\000\247\000\001\000\066\001\
    \254\255\255\255\076\001\007\000\250\255\005\000\005\000\246\255\
    \229\255\038\000\245\255\023\000\241\255\240\255\239\255\228\255\
    \235\255\039\000\032\000\234\255\043\000\055\000\070\000\233\255\
    \227\255\232\255\061\000\067\000\230\255\223\255\000\000\059\000\
    \222\255\106\000\096\000\221\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\035\000\036\000\036\000\255\255\255\255\036\000\
    \255\255\036\000\036\000\036\000\036\000\255\255\255\255\255\255\
    \029\000\036\000\036\000\255\255\255\255\255\255\036\000\036\000\
    \255\255\255\255\255\255\036\000\004\000\003\000\002\000\001\000\
    \255\255\255\255\001\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\255\255\255\255\255\255\000\000\000\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\000\000\000\000\000\000\
    \255\255\255\255\255\255\000\000\000\000\000\000\255\255\255\255\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\000\000\255\255\255\255\000\000\
    \000\000\255\255\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\255\255\255\255\000\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\030\000\030\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \030\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \020\000\019\000\024\000\026\000\008\000\025\000\005\000\017\000\
    \032\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\046\000\006\000\013\000\015\000\014\000\000\000\
    \000\000\002\000\002\000\002\000\002\000\002\000\028\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\029\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\000\000\016\000\045\000\000\000\000\000\
    \000\000\027\000\035\000\065\000\023\000\010\000\009\000\063\000\
    \049\000\012\000\057\000\040\000\003\000\022\000\018\000\048\000\
    \007\000\062\000\058\000\053\000\011\000\052\000\002\000\047\000\
    \043\000\041\000\036\000\039\000\004\000\061\000\021\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\037\000\042\000\044\000\050\000\038\000\051\000\056\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\054\000\055\000\059\000\060\000\002\000\064\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\066\000\067\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\000\000\000\000\
    \033\000\000\000\002\000\000\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\000\000\000\000\000\000\000\000\002\000\000\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\034\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\030\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\030\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\016\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\017\000\255\255\255\255\
    \255\255\000\000\027\000\062\000\000\000\000\000\000\000\003\000\
    \011\000\000\000\009\000\037\000\000\000\000\000\000\000\012\000\
    \000\000\003\000\007\000\010\000\000\000\010\000\002\000\012\000\
    \018\000\022\000\035\000\038\000\000\000\004\000\000\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\023\000\041\000\043\000\049\000\023\000\050\000\052\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\053\000\054\000\058\000\059\000\002\000\063\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\028\000\065\000\066\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\255\255\255\255\
    \000\000\255\255\028\000\255\255\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\029\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\255\255\255\255\255\255\255\255\029\000\255\255\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read lexbuf =
   __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 27 "a2.mll"
                      ( EOF )
# 215 "a2.ml"

  | 1 ->
let
# 28 "a2.mll"
               n
# 221 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 28 "a2.mll"
                 (INT (int_of_string (remPlus n)) )
# 225 "a2.ml"

  | 2 ->
# 29 "a2.mll"
               (read lexbuf)
# 230 "a2.ml"

  | 3 ->
# 30 "a2.mll"
        (BOOL true)
# 235 "a2.ml"

  | 4 ->
# 31 "a2.mll"
        (BOOL false)
# 240 "a2.ml"

  | 5 ->
# 32 "a2.mll"
          (ABS)
# 245 "a2.ml"

  | 6 ->
# 33 "a2.mll"
        (PLUS)
# 250 "a2.ml"

  | 7 ->
# 34 "a2.mll"
        (MINUS)
# 255 "a2.ml"

  | 8 ->
# 35 "a2.mll"
        (TIMES)
# 260 "a2.ml"

  | 9 ->
# 36 "a2.mll"
          (DIV)
# 265 "a2.ml"

  | 10 ->
# 37 "a2.mll"
          (REM)
# 270 "a2.ml"

  | 11 ->
# 38 "a2.mll"
        (TILDA)
# 275 "a2.ml"

  | 12 ->
# 39 "a2.mll"
        (LP)
# 280 "a2.ml"

  | 13 ->
# 40 "a2.mll"
        (RP)
# 285 "a2.ml"

  | 14 ->
# 41 "a2.mll"
          (NOT)
# 290 "a2.ml"

  | 15 ->
# 42 "a2.mll"
          (CONJ)
# 295 "a2.ml"

  | 16 ->
# 43 "a2.mll"
          (DISJ)
# 300 "a2.ml"

  | 17 ->
# 44 "a2.mll"
        (EQ)
# 305 "a2.ml"

  | 18 ->
# 45 "a2.mll"
        (GT)
# 310 "a2.ml"

  | 19 ->
# 46 "a2.mll"
        (LT)
# 315 "a2.ml"

  | 20 ->
# 47 "a2.mll"
         (IF)
# 320 "a2.ml"

  | 21 ->
# 48 "a2.mll"
           (THEN)
# 325 "a2.ml"

  | 22 ->
# 49 "a2.mll"
           (ELSE)
# 330 "a2.ml"

  | 23 ->
# 50 "a2.mll"
          (FI)
# 335 "a2.ml"

  | 24 ->
# 51 "a2.mll"
        (COMMA)
# 340 "a2.ml"

  | 25 ->
# 52 "a2.mll"
           (PROJ)
# 345 "a2.ml"

  | 26 ->
# 53 "a2.mll"
          (DEF)
# 350 "a2.ml"

  | 27 ->
# 54 "a2.mll"
         (IN)
# 355 "a2.ml"

  | 28 ->
# 55 "a2.mll"
          (END)
# 360 "a2.ml"

  | 29 ->
# 56 "a2.mll"
         (BACKSLASH)
# 365 "a2.ml"

  | 30 ->
# 57 "a2.mll"
        (SEMICOLON)
# 370 "a2.ml"

  | 31 ->
# 58 "a2.mll"
        (DOT)
# 375 "a2.ml"

  | 32 ->
# 59 "a2.mll"
         (PARALLEL)
# 380 "a2.ml"

  | 33 ->
# 60 "a2.mll"
          (LET)
# 385 "a2.ml"

  | 34 ->
# 61 "a2.mll"
            (LOCAL)
# 390 "a2.ml"

  | 35 ->
let
# 62 "a2.mll"
           a
# 396 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 62 "a2.mll"
             (ID a)
# 400 "a2.ml"

  | 36 ->
let
# 63 "a2.mll"
         a
# 406 "a2.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 63 "a2.mll"
           (raise (InvalidToken a))
# 410 "a2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

;;

