exception InvalidSet;;
exception NonBaseTypeResult;;
exception TypeError;;


(* Types of the language *)
type uMember =  UEmptyWord | UMember of string;;
type uSet = UEmpty | UTuple of uMember list;;

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
  ;;

let (sets : uTerm list ref) = ref [];;
let (k : int ref) = ref 0;;


(*Type checking function*)
let rec typeOf e = match e with
    USet (n) -> USET
  | UInSet (b) -> USET
  | UUnion (e1,e2) ->
    ( match (typeOf e1) , (typeOf e2) with
        USET, USET -> USET
      | _ -> raise TypeError)
  | UIntersect (e1,e2) ->
    ( match (typeOf e1) , (typeOf e2) with
        USET, USET -> USET
      | _ -> raise TypeError)
  | UDifference (e1,e2) ->
    ( match (typeOf e1) , (typeOf e2) with
      USET, USET -> USET
      | _ -> raise TypeError)
  | UConcatenation (e1,e2) ->
    ( match (typeOf e1) , (typeOf e2) with
      USET, USET -> USET
      | _ -> raise TypeError)
  | UKleene (e1) ->
    ( match (typeOf e1) with
      USET -> USET
      | _ -> raise TypeError)
  | UKleeneLimited (e1,e2) ->
    ( match (typeOf e1) with
      USET -> USET
      | _ -> raise TypeError)
  ;;

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

  let rec kleenestar acc l i =
    match i with
    0 -> acc
    |x -> kleenestar (union acc (concatenation acc l)) l (x-1);;

  let set_to_kleeneable s =
    let rs = List.rev s in
    let rec stk_rec s acc =
      match s with
      | [] -> acc
      | UEmptyWord::tl -> stk_rec tl acc
      | UMember(x)::tl -> stk_rec tl (x::acc)
    in stk_rec rs [];;

    let kleene s i =
       match s with
        | USet(UEmpty) -> USet(UTuple([UEmptyWord]))
        | USet(UTuple(l)) -> USet(UTuple(kleenestar [UEmptyWord] l i))
        | _ -> raise InvalidSet;;

let limit_kleene s i =
  let length_limit l len =
    let pred s =
      match s with
        UEmptyWord -> 0 = len
      | UMember(x) -> (String.length x) = len
    in List.filter pred l
  in match s with
    | USet(UEmpty) -> USet(UTuple([UEmptyWord]))
    | USet(UTuple(l)) -> USet(UTuple(     length_limit (kleenestar [UEmptyWord] l i) i ))
    | _ -> raise InvalidSet;;

    let default_kleene s =
      match s with
        | USet(UEmpty) -> kleene s 1
        | USet(UTuple(l)) -> (
          let sanitated = set_to_kleeneable l in
          let c = List.length sanitated in
            match c with
            | 1 -> kleene s 50
            | _ -> (
              let fc = float_of_int c in
              let n = int_of_float(floor ((log (1. -. 3000. +. (3000. *. fc))) /. (log fc))) in
                kleene s n
              )
          )
        | _ -> raise InvalidSet;;

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
               let n = int_of_float(floor (((log (1. -. fi +. (fi *. fc))) /. (log fc)) +. min_float)) in
               let m = i - ((1-(pow c n)) / (1-c)) in
                let rec build_member j acc =
                  match j with
                  | -1 -> acc
                  | _ -> build_member (j-1) (String.concat "" [acc;(get_nth (s, ((m/(pow c j)) mod c)))])
                in build_member (n-1) "";;

  (* let kleene s =
    let rec build_kleene l n acc =
      match n with
      | 0 -> acc
      | 1 -> UEmptyWord::acc
      | _ -> build_kleene l (n-1) ((UMember (kleenefy l (n-1)))::acc)
    in match s with
    | USet(UEmpty) -> USet(UTuple([UEmptyWord]))
    | USet(UTuple(x)) -> USet(UTuple(build_kleene (set_to_kleeneable x) 2000 []))
    | _ -> raise InvalidSet;; *)

  (* let kleene_limit s i =

    let rec build_kleene l n acc =
      match n with
      | 0 -> acc
      | 1 -> UEmptyWord::acc
      | _ -> build_kleene l (n-1) ((UMember (kleenefy l (n-1)))::acc)
    in match s with
    | USet(UEmpty) when i > 0 -> USet(UTuple([UEmptyWord]))
    | USet(UTuple(x)) ->
      (let sanitated = (set_to_kleeneable x) in
      let c = List.length sanitated in
        match c with
        | 1 -> USet(UTuple(build_kleene sanitated (i+1) []))
        | _ -> let count = ((1 - (pow c (i + 1))) / (1 - c)) in
          USet(UTuple(build_kleene sanitated count [])))
    | _ -> raise InvalidSet;; *)

(* Eval Function *)
let rec evalBig e = match e with
  | USet (UTuple([])) -> USet(UEmpty)
  | USet (x) -> order (setify e)
  | UInSet (x) -> evalBig (get_nth (!sets, x-1))
  | UUnion (s1, s2) -> let v1 = evalBig s1 in
                       let v2 = evalBig s2 in
                        (match (v1,v2) with
                          | USet(UTuple(n)), USet(UTuple(m)) -> evalBig (USet(UTuple(union n m)))
                          | USet(UEmpty), USet(UTuple(m)) -> evalBig (USet(UTuple(m)))
                          | USet(UTuple(n)), USet(UEmpty) -> evalBig (USet(UTuple(n)))
                          | _ -> raise InvalidSet)

  | UIntersect (s1, s2) -> let v1 = evalBig s1 in
                           let v2 = evalBig s2 in
                            (match (v1,v2) with
                              | USet(UTuple(n)), USet(UTuple(m)) -> evalBig (USet(UTuple(intersect n m)))
                              | USet(UEmpty), USet(UTuple(m)) -> evalBig (USet(UEmpty))
                              | USet(UTuple(n)), USet(UEmpty) -> evalBig (USet(UEmpty))
                              | _ -> raise InvalidSet)

  | UDifference (s1, s2) -> let v1 = evalBig s1 in
                            let v2 = evalBig s2 in
                              (match (v1,v2) with
                                | USet(UTuple(n)), USet(UTuple(m)) -> evalBig (USet(UTuple(difference n m)))
                                | USet(UEmpty), USet(UTuple(m)) -> evalBig (USet(UEmpty))
                                | USet(UTuple(n)), USet(UEmpty) -> evalBig (USet(UTuple(n)))
                                | _ -> raise InvalidSet)

  | UConcatenation (s1, s2) -> let v1 = evalBig s1 in
                               let v2 = evalBig s2 in
                                (match (v1,v2) with
                                  | USet(UTuple(n)), USet(UTuple(m)) -> evalBig (USet(UTuple(concatenation n m)))
                                  | USet(UEmpty), USet(UTuple(m)) -> evalBig (USet(UEmpty))
                                  | USet(UTuple(n)), USet(UEmpty) -> evalBig (USet(UEmpty))
                                  | _ -> raise InvalidSet)

  | UKleene (x) -> evalBig (default_kleene (evalBig x))
  | UKleeneLimited (s, i) -> evalBig (limit_kleene (evalBig s) i)
;;

let rec eval e =
  match e with
  | [] -> []
  | hd::tl -> (limit (order (evalBig hd)))::(eval tl);;


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
