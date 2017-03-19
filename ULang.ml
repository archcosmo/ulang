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

let (sets : uTerm list ref) = ref [];;
let (k : int ref) = ref 0;;


(*Type checking function*)
(* let rec typeOf e = match e with
  USet () *)

let evalInputs inp =
  let rec addInSets = function
    | [] -> ()
    | USet (x)::tl -> sets:= !sets@[USet (x)]; addInSets tl
    | _ -> raise InvalidSet
  in

  let (inpSets, num) = inp in
    k := num;
    addInSets inpSets
;;

let pow x n =
  let rec pow_rec acc n =
    match n with
    | 0 -> 1
    | 1 -> acc
    | _ -> (pow_rec (x * acc) (n-1))
  in pow_rec x n;;

let rec get_nth = function
  | [], _           -> raise (Failure "get_nth : index is greater than list length")
  | _, n when n < 0 -> raise (Failure "get_nth : negative index")
  | x::_, 0         -> x
  | x::xs, n        -> get_nth(xs, n-1);;

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


 let rec concatenation l1 l2 =
  let rec prefix m l =
    match m with
    | UEmptyWord -> l
    | UMember(w) -> (match l with
            | [] -> []
            | UEmptyWord::tl -> UMember(w)::(prefix m tl)
            | UMember(x)::tl -> UMember(String.concat "" [w;x])::(prefix m tl)
            )
  in match l1 with
    | [] -> []
    | hd::tl -> List.append (prefix hd l2) (concatenation tl l2)
  ;;


 (* let kleenestar l1 = ;;
 let complement l1 = ;; *)

 let setify e =
  let rec remove_duplicate = function
    | []                              -> []
    | hd::[]                          -> [hd]
    | hd::tl when not (in_list hd tl) -> hd::(remove_duplicate tl)
    | hd::tl                          -> remove_duplicate tl
  in
  match e with
    | USet(UTuple(x)) -> USet(UTuple(remove_duplicate x))
    | USet(UEmpty)    -> e
    | _ -> raise InvalidSet;;


  let order s =
    let comp s1 s2 = match s1, s2 with
    | UEmptyWord, UEmptyWord ->  0
    | UEmptyWord, UMember(y) -> -1
    | UMember(x), UEmptyWord ->  1
    | UMember(x), UMember(y) when x=y ->  0
    | UMember(x), UMember(y) when x>y ->  1
    | UMember(x), UMember(y)          -> -1
    in
    match s with
      | USet(UTuple(x)) -> USet(UTuple(List.sort comp x))
      | USet(UEmpty)    -> s
      | _ -> raise InvalidSet;;

  let limit s =
    let rec list_limit l n =
      match l,n with
      | [], _              -> []
      | hd::tl, x when x>0 -> hd::(list_limit tl (x-1))
      | _, _               -> []
    in match s with
    | USet(UTuple(x)) -> USet(UTuple(list_limit x !k))
    | USet(UEmpty)    -> s
    | _ -> raise InvalidSet;;

  let set_to_kleeneable s =
    let rs = List.rev s in
    let rec stk_rec s acc =
      match s with
      | [] -> acc
      | UEmptyWord::tl -> stk_rec tl acc
      | UMember(x)::tl -> stk_rec tl (x::acc)
    in stk_rec rs [];;

  let kleenefy s i =
    let c = List.length s in
      match c with
      | 1 -> let rec build_member j acc =
              match j with
              | 0 -> acc
              | _ -> build_member (j-1) (String.concat "" [acc;(get_nth (s, 0))])
              in build_member (i) ""
      | _ -> let fc = float_of_int c in
             let fi = float_of_int i in
             let n = int_of_float(floor ((log (1. -. fi +. (fi *. fc))) /. (log fc))) in
             let m = i - ((1-(pow c n)) / (1-c)) in
              let rec build_member j acc =
                match j with
                | -1 -> acc
                | _ -> build_member (j-1) (String.concat "" [acc;(get_nth (s, ((m/(pow c j)) mod c)))])
              in build_member (n-1) "";;

  let kleene s =
    let rec build_kleene l n acc =
      match n with
      | 0 -> acc
      | 1 -> UEmptyWord::acc
      | _ -> build_kleene l (n-1) ((UMember (kleenefy l (n-1)))::acc)
    in match s with
    | USet(UEmpty) -> USet(UTuple([UEmptyWord]))
    | USet(UTuple(x)) -> USet(UTuple(build_kleene (set_to_kleeneable x) 2000 []))
    | _ -> raise InvalidSet;;

(* Eval Function *)
let rec evalBig e = match e with
  | USet (x) -> order (setify e)
  | UInSet (x) -> evalBig (get_nth (!sets, x-1))
  | UUnion (s1, s2) -> let v1 = evalBig s1 in
                       let v2 = evalBig s2 in
                        (match (v1,v2) with
                          | USet(UTuple(n)), USet(UTuple(m)) -> USet(UTuple(union n m))
                          | USet(UEmpty), USet(UTuple(m)) -> USet(UTuple(m))
                          | USet(UTuple(n)), USet(UEmpty) -> USet(UTuple(n))
                          | _ -> raise InvalidSet)

  | UIntersect (s1, s2) -> let v1 = evalBig s1 in
                           let v2 = evalBig s2 in
                            (match (v1,v2) with
                              | USet(UTuple(n)), USet(UTuple(m)) -> USet(UTuple(intersect n m))
                              | USet(UEmpty), USet(UTuple(m)) -> USet(UEmpty)
                              | USet(UTuple(n)), USet(UEmpty) -> USet(UEmpty)
                              | _ -> raise InvalidSet)

  | UDifference (s1, s2) -> let v1 = evalBig s1 in
                            let v2 = evalBig s2 in
                              (match (v1,v2) with
                                | USet(UTuple(n)), USet(UTuple(m)) -> USet(UTuple(difference n m))
                                | USet(UEmpty), USet(UTuple(m)) -> USet(UEmpty)
                                | USet(UTuple(n)), USet(UEmpty) -> USet(UTuple(n))
                                | _ -> raise InvalidSet)

  | UConcatenation (s1, s2) -> let v1 = evalBig s1 in
                               let v2 = evalBig s2 in
                                (match (v1,v2) with
                                  | USet(UTuple(n)), USet(UTuple(m)) -> USet(UTuple(concatenation n m))
                                  | USet(UEmpty), USet(UTuple(m)) -> USet(UEmpty)
                                  | USet(UTuple(n)), USet(UEmpty) -> USet(UEmpty)
                                  | _ -> raise InvalidSet)

  | UKleene (x) -> kleene (evalBig x);;

let eval e = limit (order (evalBig e));;


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
