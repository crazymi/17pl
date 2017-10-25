(* type definition *)
type require = id * (cond list)
and cond
    = Items of gift list
    | Same of id
    | Common of cond * cond
    | Except of cond * gift list
and gift = int
and id = A | B | C | D | E
and state = id * gift list

(* print function *)
let print_list l = List.iter print_int l; print_newline()
let print_id i = match i with
               | A -> print_string "A"
               | B -> print_string "B"
               | C -> print_string "C"
               | D -> print_string "D"
               | E -> print_string "E"
let print_state l = List.iter
  (fun (i,gl) ->
    print_id i;
    print_string ": ";
    print_list gl
  ) l
let print_bool b = if b then print_string "true" else print_string "false" ; print_newline()

(* list/state function *)

(* if value is in the list return true else false *)
let rec in_list : 'a list -> 'a -> bool = fun l v ->
  match l with
  | [] -> false
  | hd::[] -> hd == v
  | hd::tl -> if hd == v then true else (in_list tl v)

let sort_list : gift list -> gift list = fun l ->
  let s a b = if a=b then 0 else if a>b then 1 else -1 in
  List.sort s l

let rec remove_dup_list : int list -> int list = fun l ->
  match l with
  | hd::tl -> if(in_list tl hd) then
                remove_dup_list tl
              else
                hd::(remove_dup_list tl)
  | [] -> l

let sort_state : state list -> state list = fun sl ->
  List.map (fun (id,gl) -> (id, sort_list (remove_dup_list gl))) sl

(* support function *)

(* item value to state *)
let item_to_state : require -> state -> state =
  fun (id, cl) (id', gl) ->
    let gl' = 
      List.fold_left (
        fun g c -> match c with
        | Items xl -> g@xl
        | _ -> g
      ) gl cl
    in
    if id=id' then
      (id, gl')
    else
      failwith "item_to_state: id not equal"


(* main function *)
let shoppingList : require list -> (id * gift list) list =
  fun reqList ->
    let initStateList = [(A,[]); (B,[]); (C,[]); (D,[]); (E,[])] in
    let stateList : state list = initStateList in
    let result = ref stateList in
    result := List.map2 item_to_state reqList !result;
    result := sort_state !result;
    !result

(* test code *)
let reqA : require = (A, [Items [1; 2]; Common (Same B, Same C); Items [1;3;4]])
let reqB : require = (B, [Common (Same C, Items [2; 3])])
let reqC : require = (C, [Items [1]; Except (Same A, [3])])
let reqD : require = (D, [])
let reqE : require = (E, [])

let myReq = [reqA;reqB;reqC;reqD;reqE]

let _ = 
  let x = shoppingList(myReq) in
  print_state x

(*
let _ = print_bool (in_list [1;4;5;6] 3)
let _ = 
  let x = [1;2;3;4;1;2;3;5;1;3] in
  print_list (sort_list x);
  let rec remove_dup_list : int list -> int list = fun l ->
    match l with
    | hd::tl -> if(in_list tl hd) then
                  remove_dup_list tl
                else
                  hd::(remove_dup_list tl)
    | [] -> []
  in
  print_list (sort_list (remove_dup_list x))
let _ =
  let initStateList = [(A,[4;3;2;1]); (B,[1;1;1;2;2;2]); (C,[]); (D,[]); (E,[5;5;5;3;3;3;1;1;1;1;2])] in
  print_state initStateList;
  print_state (sort_state initStateList)
  *)
