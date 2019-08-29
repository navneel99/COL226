%{
  open A6
%}

%token <string> ID
%token <int> INT
%token <string> FNAME
%token LP RP COMMA COLON EQ RET EOF

%start a6_parser
%type <A6.arg> a6_parser

%%

a6_parser:
  | funCall_parser EOF { $1 }
;

funCall_parser:
  | FNAME { Frame(Procedure($1),(ArgI ("null",0),ArgI ("null",0)),LocalV [],AllV [], FrameList [])}
  | FNAME LP const_parser COMMA const_parser RP { let l = A6.getArgs $1 in ( match $3,$5 with
                                                                              | ArgI (x1,x2), ArgI (y1,y2) -> Frame(Procedure($1),(ArgI (List.nth l 0,x2),ArgI(List.nth l 1, y2)) ,LocalV[],AllV[],FrameList[]) 
                                                                              | ArgI (x1,x2), ArgS (y1,y2) -> Frame(Procedure($1),(ArgI (List.nth l 0,x2),ArgS(List.nth l 1, y2)) ,LocalV[],AllV[],FrameList[])
                                                                              | ArgS (x1,x2), ArgI (y1,y2) -> Frame(Procedure($1),(ArgS (List.nth l 0,x2),ArgI(List.nth l 1, y2)) ,LocalV[],AllV[],FrameList[])
                                                                              | ArgS (x1,x2), ArgS (y1,y2) -> Frame(Procedure($1),(ArgS (List.nth l 0,x2),ArgS(List.nth l 1, y2)) ,LocalV[],AllV[],FrameList[])
                                                                            ) }
  | assign_parser { $1 }

;
assign_parser:
  | ID COLON EQ const_parser {( match $4 with
                                          |  ArgI (y1,y2) -> ArgI($1,y2) 
                                          |  ArgS (y1,y2) -> ArgS($1,y2) )}
  | ret_parser { $1 }
;
ret_parser:
  | RET { ArgI("null",0) }
  | const_parser { $1 }

const_parser:
  | ID  { ArgS ("dummy",$1) }
  | INT { ArgI ("dummy",$1) }
;