(* Types of the language *)
type uMember =  UEmptyWord | UMember of string
type uSet = UEmpty | UTuple of uMember list
type uType = USET | UINT;;

(* Grammar of the language *)
type uTerm =
  | USet of uSet
  | UInSet of int
  | UUnion of uTerm * uTerm
  | UIntersect of uTerm * uTerm
  | UDifference of uTerm * uTerm
  | UConcatenation of uTerm * uTerm
  | UKleene of uTerm
  | UKleeneLimited of uTerm * int


val typeOf : uTerm -> uType
val evalInputs : uTerm list * int -> unit
val eval : uTerm list -> uTerm list
val print_res : uTerm -> unit
