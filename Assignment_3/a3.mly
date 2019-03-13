%{
    open A1
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
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
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
  | constant { $1 }
;

rem_par:
  | and_expr COMMA and_expr{ (2,[$1;$3]) }
  | and_expr COMMA rem_par {let x,y = $3 in (x+1,$1::y)}
; 

constant:
  | ID                     { Var($1) }
  | INT                    { N($1) }
  | BOOL                    { B($1) }
  | LP and_expr RP              { InParen($2) }
;


