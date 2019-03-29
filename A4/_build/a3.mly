%{
    open A1
    exception TypeError
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF
/* %token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF */
%start  def_parser exp_parser
%type <A1.definition> def_parser  /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/* Implement the grammar rules for expressions, which may use the parser for definitions */
exp_parser:
  | and_expr EOF { $1 }
;
and_expr:
  | and_expr DISJ or_expr { Disjunction($1,$3) }
  | or_expr               { $1 }
;
or_expr:
  | or_expr CONJ not_expr { Conjunction($1,$3) }
  | not_expr              { $1 }
;

not_expr:
  | NOT constant          { Not($2) }
  | eq_expr             { $1 }
;

eq_expr:
  | eq_expr EQ lt_expr   { Equals($1,$3) }
  | lt_expr              { $1 }
;
lt_expr:
  | lt_expr LT EQ gt_expr { LessTE($1,$4) }
  | lt_expr LT gt_expr  { LessT($1,$3) }
  | gt_expr            { $1 }
;

gt_expr:
  | gt_expr GT EQ add_sub_expr { GreaterTE($1,$4) }
  | gt_expr GT add_sub_expr   { GreaterT($1,$3) }
  | add_sub_expr              { $1 }
;

add_sub_expr:
  | add_sub_expr MINUS rem_expr { Sub($1,$3) }
  | add_sub_expr PLUS rem_expr  { Add($1,$3) }
  | rem_expr               { $1 }
;

rem_expr:
  | rem_expr REM abs_expr { Rem($1,$3) }
  | rem_expr TIMES abs_expr { Mult($1,$3) }
  | rem_expr DIV abs_expr { Div($1,$3) }
  | abs_expr             { $1 }
;

/* mult_expr:
  | mult_expr TIMES div_expr  { Mult($1,$3) }
  | div_expr               { $1 }
;

div_expr:
  | div_expr DIV abs_expr  { Div($1,$3) }
  | abs_expr               { $1 }
; */

abs_expr:
  | TILDA abs_expr         { Negative($2)}
  | ABS abs_expr         { Abs($2) }
  | ifte_expr              { $1 }
;
/* neg_expr:
  | TILDA ifte_expr        { Negative($2) }
  | ifte_expr             { $1 } */
;
 ifte_expr:
  | IF and_expr THEN and_expr ELSE and_expr FI { IfThenElse($2,$4,$6) }
  | proj_expr                         { $1 }
; 

 proj_expr:
  | PROJ LP INT COMMA INT RP tup_expr { Project(($3,$5),$7)}
  | tup_expr { $1 }
;

tup_expr:
  | LP rem_par RP { let a,b = $2 in Tuple(a,b) }  
  | LP RP    {Tuple(0,[])}
  | funCall_expr { $1 }
;

rem_par:
  | and_expr COMMA and_expr{ (2,[$1;$3]) }
  | and_expr COMMA rem_par {let x,y = $3 in (x+1,$1::y)}
; 

funCall_expr:
  | funAbs_expr LP and_expr RP {FunctionCall($1,$3)}
  | funAbs_expr { $1 }
;
funAbs_expr:
  | BACKSLASH ID DOT let_expr { FunctionAbstraction($2,$4)}
  | let_expr { $1 }
;

let_expr:
  | LET local_def IN and_expr END { Let($2, $4) }
  | paren_expr { $1 }
;
paren_expr:
  | LP and_expr RP { InParen($2) }
  | constant { $1 }
;
constant:
  | ID                     { Var($1) }
  | INT                    { N($1) }
  | BOOL                    { B($1) }
  /* | LP and_expr RP              { InParen($2) } */
;

/* Implement the grammar rules for definitions, which may use the parser for expression  */
def_parser:
  | local_def EOF    { $1 }
  /* | LET ID IN and_expr END EOF { Simple($2, $4) } */
;


local_def:
  | LOCAL local_def IN seq_def END { Local($2, $4) }
  | seq_def { $1 }
;

seq_def:
  /* | SEMICOLON {Sequence([])}
  | PARALLEL {Parallel([])} */
  /* | simple_def SEMICOLON simple_def { Sequence([$1; $3]) }
  | simple_def PARALLEL simple_def { Parallel([$1;$3]) } */
  | simple_def { $1 }
  | seq_def PARALLEL simple_def { match $1 with
                                    | Parallel(x) -> Parallel (x @ [$3])
                                    | Sequence(x) -> Parallel($1 :: [$3])
                                    | _ -> Parallel([$1] @ [$3] ) 
                                     }
  | seq_def SEMICOLON simple_def { match $1 with 
                                    | Sequence(x) -> Sequence (x @ [$3])
                                    | Parallel(x) -> Sequence($1 :: [$3])
                                    | _-> Sequence( [$1] @ [$3] )  }
; 

/* paral_def:
  | local_def PARALLEL local_def { Parallel([$1;$3])}
  | local_def PARALLEL paral_def { match $3 with
                                    | Parallel(x) -> Parallel ($1::x) }
  | local_def { $1 }
; */


simple_def:
  | DEF ID EQ and_expr { Simple($2, $4) }
  | LP local_def RP  { $2 }
;