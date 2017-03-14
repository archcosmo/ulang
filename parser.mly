/* File parser.mly */
%{
    open Toy
%}
%token <int> INT
%token <string> WORD
%token LBRACE RBRACE
%token EMPTYWORD
%token EOF

%left UNION INTERSECT DIFFERENCE CONCATENATION /* lowest precedence */
%nonassoc  KLEENSTAR COMPLEMENT /* highest precedence */

%start parser_main             /* the entry point */

%%
parser_main: expr EOF { $1 }
;
type_spec: SET { ?? }
    | BTYPE     { ToyBool }
    | type_spec FUNTYPE type_spec { ToyFun ($1,$3) }
    | LPAREN type_spec RPAREN {$2}
;

expr:
 | LBRACE set RBRACE { UTuple ($2) }
;

set:
  | WORD { UMember($1) }
  | WORD COMMA set { UMember($1)::$3 }
;
