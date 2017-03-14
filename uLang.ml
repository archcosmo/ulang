(* Types of the language *)
type uMember =  UMember of string;;
type uSet = UEmpty | USingleton of uMember | UTuple of uMember list;;


(* Grammar of the language *)
type uTerm =
  | USet of uSet
  | UUnion of uTerm * uTerm
  | UIntersect of uTerm * uTerm
  | UComplement of uTerm
  | UDifference of uTerm * uTerm
  | UConcatenation of uTerm * uTerm
  | UKleene of uTerm
  ;;

let (sets : uSet list) = [];;
let (k : int ref) = ref 0;;
