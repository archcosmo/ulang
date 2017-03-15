/* File parser.mly */
%{
    open ULang
%}
%token <int> INT
%token <int> IN
%token <string> WORD
%token LBRACE COMMA RBRACE
%token KLEENESTAR
%token UNION
%token INTERSECT
%token CONCATENATION
%token DIFFERENCE
%token COMPLEMENT
%token EMPTYWORD
%token EOF

%left UNION INTERSECT DIFFERENCE CONCATENATION /* lowest precedence */
%nonassoc  KLEENESTAR COMPLEMENT /* highest precedence */

%start parser_main             /* the entry point */
%type <ULang.uTerm> parser_main
%%
parser_main: expr EOF { $1 }
;

expr:
    set                         { USet ($1) }
  | IN                          { UInSet ($1) }
  | expr UNION expr             { UUnion ($1, $3) }
  | expr INTERSECT expr         { UIntersect ($1, $3) }
  | expr CONCATENATION expr     { UConcatenation ($1, $3) }
  | expr KLEENESTAR             { UKleene ($1) }
  | expr DIFFERENCE expr        { UDifference ($1, $3) }
  | expr COMPLEMENT             { UComplement ($1) }
;

set:
    LBRACE RBRACE               { UEmpty }
  | LBRACE setmembers RBRACE    { UTuple ($2) }
;

setmembers:
    EMPTYWORD                   { [UEmptyWord] }
  | WORD                        { [UMember($1)] }
  | EMPTYWORD COMMA setmembers  { UEmptyWord::$3 }
  | WORD COMMA setmembers       { UMember($1)::$3 }
;
