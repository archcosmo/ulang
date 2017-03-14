/* File parser.mly */
%{
    open Toy
%}
%token <int> INT
%token <string> IDENT
%token LBRACE RBRACE FOLLOWEDBY
%token EMPTYWORD KLEENESTAR
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
expr: INT                      { TmNum $1 }
 | FALSE                       { TmBool false }
 | TRUE                        { TmBool true }
 | IDENT                       { TmVar $1 }
 | LET LPAREN IDENT COLON type_spec RPAREN EQUALS expr IN expr { TmLet ($3, $5, $8, $10) }
 | expr LPAREN expr RPAREN     { TmApp ($1, $3) }
 | expr PLUS expr              { TmPlus ($1, $3) }
 | expr LESSTHAN expr          { TmLessThan ($1, $3) }
 | expr EQUALS EQUALS expr      { TmEqual ($1, $4) }
 | IF LPAREN expr RPAREN THEN expr ELSE expr { TmIf ($3, $6, $8) }
 | LAMBDA LPAREN IDENT COLON type_spec RPAREN expr {TmAbs ($3, $5, $7) }
 | LPAREN expr RPAREN          { $2 }
;
