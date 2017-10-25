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
let print_list l = List.iter print_int l
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
    print_list gl;
    print_newline()
  ) l

let rec print_cond c = match c with
                  | Items l ->  print_list l;print_string " "
                  | Same i -> print_id i
                  | Common (c1, c2) -> print_string "("; print_cond c1; print_string "^"; print_cond c2;print_string ")"
                  | Except (c1, gl) -> print_string "("; print_cond c1; print_string "-"; print_list gl;print_string ")"

let print_req = fun (i, cl) -> print_id i; print_newline(); List.iter print_cond cl;print_newline()

let print_bool b = if b then print_string "true" else print_string "false" ; print_newline()

(* list/state function *)
let id_to_int i = match i with
                | A -> 0
                | B -> 1
                | C -> 2
                | D -> 3
                | E -> 4

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

let drop_list : 'a list -> 'a -> 'a list = fun l v ->
  List.filter (fun x -> x != v) l

let common_list : 'a list -> 'a list -> 'a list = fun l1 l2 ->
  sort_list (remove_dup_list (List.filter (fun x -> in_list l1 x) l2))
let except_list : 'a list -> 'a list -> 'a list = fun l1 l2 ->
  sort_list (remove_dup_list (List.filter (fun x -> not (in_list l2 x)) l1))

let drop_list_items : require -> require  = fun (i, l) ->
  (i, List.fold_left (
    fun a b -> match b with
              | Items _ -> a
              | _ -> List.append a [b]
  ) [] l )

(* support function *)

(* unnecessary
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
*)


(* main function *)
let shoppingList : require list -> (id * gift list) list =
  fun reqList ->
    let initStateList = [(A,[]); (B,[]); (C,[]); (D,[]); (E,[])] in
    let stateList : state list = initStateList in
    let result = ref stateList in

    (* unnecessary code
    let rList = ref reqList in
    (* items -> state *)
    result := List.map2 item_to_state reqList !result;
    (* remove dup and sort *)
    result := sort_state !result;
    (* drop ITEMS in req list *)
    rList := List.map drop_list_items !rList;
    (* print require list *)
    List.iter print_req !rList;
    *)

    let rec calc_cond : cond -> gift list = fun c ->
      match c with
      | Same i ->
        let x : state list = !result in
        let ii = id_to_int i in
        let iState : state = List.nth x ii in
        begin
          match iState with
          | (_, gl) -> gl
        end
      | Common (c1, c2) -> common_list (calc_cond c1) (calc_cond c2)
      | Except (c', gl) -> except_list (calc_cond c') gl
      | Items gl -> gl
    in

    let chkIter = fun beforeResult ->
      let refResult = ref [] in
      List.iter (
        fun (id, cl) -> (* cond list *)
          let x : state list = beforeResult in
          let ii = id_to_int id in
          let currentState : state = List.nth x ii in
          let res = match currentState with
          | (_, gl) -> List.fold_left (
                       fun newState cnd ->
                         List.append newState (calc_cond cnd)
                     ) gl cl
          in
          let sortRes = sort_state [(id, res)] in
          refResult := List.append !refResult sortRes
      ) reqList;
      !refResult
    in

    let quit_flag = ref false in
    while (not !quit_flag) do
      let beforeResult = !result in
      (* for debug
      print_state !result; print_string "---------"; print_newline();
      *)
      result := chkIter !result;
      quit_flag := (beforeResult = !result)
    done;

    !result

(* below is test code *)
let reqA : require = (A, [Items [1; 2]; Common (Same B, Same C)])
let reqB : require = (B, [Common (Same C, Items [2; 3])])
let reqC : require = (C, [Items [1]; Except (Same A, [3])])
let reqD : require = (D, [])
let reqE : require = (E, [])

let myReq = [reqA;reqB;reqC;reqD;reqE]

let _ = 
  print_string "---shoppingList---";print_newline();
  let x = shoppingList(myReq) in
  print_state x

(*
let _ =
  print_string "---anotherTest---";print_newline();
  let x = reqA in
  let y = drop_list_items x in
  print_req y;
  List.iter print_req myReq
*)


(* several function test *)

(*
let _ =
  let x = [1;2;3;4;5;5;5;5;5] in
  let y = [3;4;5;6;7;8;9;3;4;3;4] in
  let z = common_list x y in
  let w = except_list x y in
  let k = except_list y x in
  print_list z; print_newline();
  print_list w; print_newline();
  print_list k; print_newline()
*)

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
