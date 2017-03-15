(* Types of the language *)
type uMember =  UEmptyWord | UMember of string
type uSet = UEmpty | UTuple of uMember list

(* Grammar of the language *)
type uTerm =
  | USet of uSet
  | UInSet of int
  | UUnion of uTerm * uTerm
  | UIntersect of uTerm * uTerm
  | UComplement of uTerm
  | UDifference of uTerm * uTerm
  | UConcatenation of uTerm * uTerm
  | UKleene of uTerm

val evalInputs : uTerm list * int -> unit
val eval : uTerm -> uTerm
val print_res : uTerm -> unit
