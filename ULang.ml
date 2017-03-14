exception InvalidSet;;
exception NonBaseTypeResult;;


(* Types of the language *)
type uMember =  UEmptyWord | UMember of string;;
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

let (sets : uTerm list) = [];;
let (k : int ref) = ref 0;;


(*Type checking function*)
(* let rec typeOf e = match e with
  USet () *)


let rec get_nth = function
  |[], _ -> raise (Failure "get_nth")
  |_, n when n < 0 -> raise (Failure "get_nth")
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
    | hd::tl when not (in_list hd l2) -> hd::union tl l2
    | hd::tl                          -> union tl l2;;

let rec intersect l1 l2 =
  match l1 with
    | [] -> []
    | hd::tl when (in_list hd l2) ->  hd::intersect tl l2
    | hd::tl                      ->  intersect tl l2;;

let rec difference l1 l2 =
  match l1 with
    | [] -> []
    | hd::tl when not (in_list hd l2) ->  hd::difference tl l2
    | hd::tl                          ->  difference tl l2;;

let rec prefix m l =
  match m with
  | UEmptyWord -> l
  | UMember(w) -> (match l with
          | [] -> []
          | UEmptyWord::tl -> UMember(w)::(prefix m tl)
          | UMember(x)::tl -> UMember(String.concat "" [w;x])::(prefix m tl)
          );;

(* how to handle EMPTYWORD ?? *)
 let rec concatenation l1 l2 =
  match l1 with
    | [] -> []
    | hd::tl -> List.append (prefix hd l2) (concatenation tl l2)
    ;;


 (* let kleenestar l1 = ;;
 let complement l1 = ;; *)

(* Eval Function *)
let rec eval e = match e with
  | USet (x) -> e
  | UInSet (x) -> get_nth (sets, x)
  | UUnion (s1, s2) -> let v1 = eval s1 in
                       let v2 = eval s2 in
                        (match (v1,v2) with
                          | USet(UTuple(n)), USet(UTuple(m)) -> USet(UTuple(union n m))
                          | USet(UEmpty), USet(UTuple(m)) -> USet(UTuple(m))
                          | USet(UTuple(n)), USet(UEmpty) -> USet(UTuple(n))
                          | _ -> raise InvalidSet)

  | UIntersect (s1, s2) -> let v1 = eval s1 in
                           let v2 = eval s2 in
                            (match (v1,v2) with
                              | USet(UTuple(n)), USet(UTuple(m)) -> USet(UTuple(intersect n m))
                              | USet(UEmpty), USet(UTuple(m)) -> USet(UEmpty)
                              | USet(UTuple(n)), USet(UEmpty) -> USet(UEmpty)
                              | _ -> raise InvalidSet)

  | UDifference (s1, s2) -> let v1 = eval s1 in
                            let v2 = eval s2 in
                              (match (v1,v2) with
                                | USet(UTuple(n)), USet(UTuple(m)) -> USet(UTuple(difference n m))
                                | USet(UEmpty), USet(UTuple(m)) -> USet(UEmpty)
                                | USet(UTuple(n)), USet(UEmpty) -> USet(UTuple(n))
                                | _ -> raise InvalidSet)

  | UConcatenation (s1, s2) -> let v1 = eval s1 in
                               let v2 = eval s2 in
                                (match (v1,v2) with
                                  | USet(UTuple(n)), USet(UTuple(m)) -> USet(UTuple(concatenation n m))
                                  | USet(UEmpty), USet(UTuple(m)) -> USet(UEmpty)
                                  | USet(UTuple(n)), USet(UEmpty) -> USet(UEmpty)
                                  | _ -> raise InvalidSet);;

let rec print_members l = match l with
  | [] -> ()
  | UEmptyWord::[] -> print_string ":"
  | UEmptyWord::tl -> print_string ":"; print_string ", "; print_members tl
  | UMember(hd)::[] -> print_string hd
  | UMember(hd)::tl -> print_string hd; print_string ", "; print_members tl

let print_res res = match res with
  | (USet UEmpty) -> print_string "{}"
  | (USet UTuple(l)) -> print_string "{"; print_members l; print_string "}"
  | _ -> raise NonBaseTypeResult;;
