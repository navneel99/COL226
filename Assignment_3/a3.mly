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
  /* INT   { N($1) } */
  | sub_expr  { $1 }
  | or_expr  { $1 }
  | tup_expr {$1 }
  | EOF { Done }
;
sub_expr:
  | add_expr MINUS sub_expr { Sub($1,$3) }
  | add_expr                { $1 }
;

add_expr:
  | mult_expr PLUS add_expr  { Add($1,$3) }
  | mult_expr               { $1 }
;

mult_expr:
  | div_expr TIMES mult_expr  { Mult($1,$3) }
  | div_expr               { $1 }
;

div_expr:
  | rem_expr DIV div_expr  { Div($1,$3) }
  | rem_expr               { $1 }
;
rem_expr:
  | abs_expr REM rem_expr { Rem($1,$3) }
  | abs_expr              { $1 }
;
abs_expr:
  | ABS sub_expr          { Abs($2) }
  | neg_expr              { $1 }
;
neg_expr:
  | TILDA sub_expr        { Negative($2) }
  /* | constant              { $1 } */
  | gt_expr               { $1 }
;

gt_expr:
  | gt_expr GT EQ lt_expr { GreaterTE($1,$4) }
  | gt_expr GT lt_expr   { GreaterT($1,$3) }
  | lt_expr              { $1 }
;

lt_expr:
  | lt_expr LT EQ eq_expr { LessTE($1,$4) }
  | lt_expr LT eq_expr  { LessT($1,$3) }
  | eq_expr            { $1 }
;
eq_expr:
  | eq_expr EQ constant   { Equals($1,$3) }
  | ifte_expr              { $1 }
;

or_expr:
  | and_expr CONJ or_expr { Conjunction($1,$3) }
  | and_expr              { $1 }
;
and_expr:
  | not_expr DISJ and_expr{ Disjunction($1,$3) }
  | not_expr              { $1 }
;
not_expr:
  | NOT not_expr          { Not($2) }
  | ifte_expr              { $1 }
  /* | eq_expr               { $1 } */
;

ifte_expr:
  | IF or_expr THEN main ELSE main FI { IfThenElse($2,$4,$6) }
  | constant                          { $1 }
;

/* tup_expr:
  | LP constant COMMA tup_expr RP {} */

constant:
  | ID                     { Var($1) }
  | INT                    { N($1) }
  | BOOL                    { B($1) }
  | LP main RP              { InParen($2) }
;