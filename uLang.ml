(* Types of the language *)
type Type =  UInt of int | UString of string | ULang;;

(* Grammar of the language *)
type Term =
    UNum of int
  | UVar of string
  | USet of ULang
  | UUnion of USet * USet
  | UIntersect of USet * USet
  | UComplement of USet
  | UDifference of USet * USet

  ;;
