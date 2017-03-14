(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule lexer_main = parse
    [' ' '\t' '\n']     { lexer_main lexbuf }     (* skip blanks *)

    | '{'      { LBRACE }
    | '}'      { RBRACE }
    | ','      { COMMA }

    | '*'      { KLEENESTAR }
    | 'u'      { UNION }
    | 'n'      { INTERSECT }
    | '.'      { CONCATENATION }
    | '/'      { DIFFERENCE }
    | '\''     { COMPLEMENT }

    | ':'      { EMPTYWORD }
    | '$' ['0'-'9']+ as lxm {IN(int_of_string (String.sub lxm 1 ((String.length lxm)-1) ) )}
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | ['a'-'z']+ as lxm { WORD(lxm) }
    | eof      { EOF }
