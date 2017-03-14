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


let in_list x l =
  let pred = function
    | y when y==x -> true
    | _ -> false
    in
    List.exists pred l;;

let rec intersect l1 l2 = 
  match l1 with
  | hd::tl when (in_list hd l2) ->  hd::intersect tl l2
  | hd when (in_list hd l2) -> hd;;
