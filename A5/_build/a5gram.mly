%{
  open A5
%}

%token <int> INT
%token <bool> BOOL
%token <string> ID
%token PLUS CONJ DISJ IF THEN ELSE FI CMP TIMES BACKSLASH DOT LP RP SEMICOLON EOF

%start a5_parser
%type <A5.expr> a5_parser

%%

a5_parser:
  | plus_parser EOF { $1 }
;
plus_parser:
  | plus_parser PLUS mult_parser {Plus($1,$3)}
  | mult_parser { $1 }
;

mult_parser:
  | mult_parser TIMES bool_parser { Mult($1,$3) }
  | bool_parser { $1 }
;

bool_parser:
  | bool_parser DISJ cmp_parser { Or($1,$3) }
  | bool_parser CONJ cmp_parser { And($1,$3) }
  | cmp_parser { $1 }
;

cmp_parser:
  | CMP ifte_parser {Cmp $2 }
  | ifte_parser { $1 }
;
ifte_parser:
  | IF plus_parser THEN plus_parser  ELSE plus_parser FI {If_Then_Else($2,$4,$6)}
  | funCall_parser { $1 }
;
funCall_parser:
  | funAbs_parser LP plus_parser RP {App($1,$3)}
  | funAbs_parser { $1 }
;
funAbs_parser:
  | BACKSLASH paren_parser DOT paren_parser {Lambda($2,$4)}
  | paren_parser {$1 }
;
paren_parser:
  | LP plus_parser RP {$2}
  |const_parser {$1}
;
const_parser:
  | ID {V($1) }
  | BOOL {Bool($1)}
  | INT {Integer($1)}
;