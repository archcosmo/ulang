exception InvalidSet;;


(* Types of the language *)
type uMember =  UMember of string;;
type uSet = UEmpty | UTuple of uMember list;;

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
  ;;

let (sets : USet list) = [];;
let (k : int ref) = ref 0;;

(*Type checking function*)
(* let rec typeOf e = match e with
  USet () *)


let rec get_nth = function
  |[], _ -> raise (Failure "get_nth")
  |_, n when n < 0 -> raise (Invalid_Argument "get_nth")
  | x::_, 0 -> x
  | x::xs, n -> get_nth(xs, n-1);;

let in_list x l =
  let pred = function
    | y when y=x -> true
    | _ -> false in
  List.exists pred l;;

let rec union l1 l2 =
  match l1 with
    | [] -> l2
    | x when not (in_list x l2) -> x::l2;;

(* Eval Function *)
let rec eval e = match e with
  | USet (x) -> e
  | UInSet (x) -> get_nth (sets, x)
  | UUnion (s1, s2) -> let v1 = eval s1 in
                       let v2 = eval s2 in
                        (match (v1,v2) with
                          | USet(UTuple(n)), USet(UTuple(m)) -> USet(UTuple(union n m))
                          | USet(UEmpty), USet(UTuple(m)) -> USet(UTuple(m))
                          | USet(UTuple(n)), USet(UEmpty) -> USet(UTuple(m))
                          | _ -> raise InvalidSet
                        )
