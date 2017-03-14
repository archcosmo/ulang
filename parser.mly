/* File parser.mly */
%{
    open Toy
%}
%token <int> INT
%token <int> IN
%token <string> WORD
%token LBRACE RBRACE
%token EMPTYWORD
%token EOF

%left UNION INTERSECT DIFFERENCE CONCATENATION /* lowest precedence */
%nonassoc  KLEENESTAR COMPLEMENT /* highest precedence */

%start parser_main             /* the entry point */

%%
parser_main: expr EOF { $1 }
;

expr: set                     { USet ($1) }
  | IN                        { UInSet ($1) }
  | expr UNION expr           { UUnion ($1, $3) }
  | expr INTERSECT expr       { UIntersect ($1, $3) }
  | expr CONCATENATION expr   { UConcatenation ($1, $3) }
  | expr KLEENESTAR           { UKleene ($1) }
  | expr DIFFERENCE expr      { UDifference ($1, $3) }
  | expr COMPLEMENT           { UComplement ($1) }
;

set:
  LBRACE RBRACE            { UEmpty }
  LBRACE setmembers RBRACE { UTuple ($2) }
;

setmembers:
  WORD { [UMember($1)] }
  | WORD COMMA setmembers { UMember($1)::$3 }
;
