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

(* test code from https://ropas.snu.ac.kr/phpbb/viewtopic.php?t=5974&sid=83f433d2ac938bf8998c9e4c72350508
 * and little modified *)

let _ = 
let emptyL = [(A, []); (B, []); (C, []); (D, []); (E, [])] in 
print_string "----test----"; print_newline();
print_endline "0"; 
print_bool ((shoppingList emptyL) = emptyL); 

print_endline "1"; 
print_bool ((shoppingList [ 
(A, []); (B, []); (C, []); (D, []); (E, []); 
]) = emptyL); 

print_endline "2"; 
print_bool ((shoppingList [ 
(A, [Same B]); (B, [Same C]); (C, [Same D]); (D, [Same E]); (E, [Same A]); 
]) = emptyL); 

print_endline "3"; 
print_bool ((shoppingList [ 
(A, [Items [1;2;3]]); (B, [Items [2;3;4]]); 
(C, [Items [3;4;1]]); (D, [Items [4;1;2]]); 
(E, [Items [1;2;3;1;2;3]]); 
]) = [(A, [1; 2; 3]); (B, [2; 3; 4]); (C, [1; 3; 4]); (D, [1; 2; 4]); (E, [1; 2; 3])]); 

print_endline "4"; 
print_bool ((shoppingList [ 
(A, [Items [1;2;3]]); 
(B, [Same A]); 
(C, [Same A; Items[1;2]]); 
(D, [Same A; Items[4]]); 
(E, [Same D]); 
]) = [(A, [1; 2; 3]); (B, [1; 2; 3]); (C, [1; 2; 3]); (D, [1; 2; 3; 4]); (E, [1; 2; 3; 4])]); 

print_endline "5"; 
print_bool ((shoppingList [ 
(A, [Common (Items [1;2;3], Items [2;1;3])]); 
(B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])]); 
(C, [Common (Items [1;2;3], Items [4;5;6])]); 
(D, [Common (Items [3;2;1], Items [1])]); 
(E, [Common (Items [1;2;3], Items [])]); 
]) = [(A, [1; 2; 3]); (B, [1; 2; 3]); (C, []); (D, [1]); (E, [])]); 

print_endline "6"; 
print_bool ((shoppingList [ 
(A, []);
(B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])]); 
(C, []);
(D, [Common (Items [], Items [])]); 
(E, [Common (Items [1], Items [1])]); 
]) = [(A, []); (B, [1; 2; 3]); (C, []); (D, []); (E, [1])]); 

print_endline "7"; 
print_bool ((shoppingList [ 
(A, [Except (Items [3;2;1], [3;2;1])]); 
(B, [Except (Items [2;1;3], [])]); 
(C, [Except (Items [2;1;3], [1;2;3;4;5;6])]); 
(D, [Except (Items [], [2;1;3])]); 
(E, [Except (Items [], [])]); 
]) = [(A, []); (B, [1; 2; 3]); (C, []); (D, []); (E, [])]); 

print_endline "8"; 
print_bool ((shoppingList [ 
(A, [Common (Common (Same B, Same C), Common (Same D, Same E))]); 
(B, [Common (Same C, Common (Same D, Except (Same E, [5])))]); 
(C, [Same D; Items[7;8]]); 
(D, [Except (Same E, [1;2;3])]); 
(E, [Items [1;2;3;4;5]]); 
]) = [(A, [4]); (B, [4]); (C, [4; 5; 7; 8]); (D, [4; 5]); (E, [1; 2; 3; 4; 5])]); 

print_endline "9"; 
print_bool ((shoppingList [ 
(A, [Same B; Same C]); 
(B, [Except (Same C, [1;2;3]); Same D]); 
(C, [Items [1;2;3]; Items [3;4;5]; Common (Same A, Items [6;7])]); 
(D, [Same E]); 
(E, [Same D; Items[6;8]]); 
]) = [(A, [1; 2; 3; 4; 5; 6; 8]); (B, [4; 5; 6; 8]); (C, [1; 2; 3; 4; 5; 6]); (D, [6; 8]); (E, [6; 8])]); 

print_endline "10"; 
print_bool ((shoppingList [ 
(A, [Common (Same B, Common (Except (Items [1;2;3;4;5], [1;3;5]), Same C)); Items [2;4;8]]); 
(B, [Except (Except (Except (Same A, [1]),[1;2]),[3]); Items [3;6;9]]); 
(C, [Same A; Same B; Same D; Same E]); 
(D, [Items [10]; Common (Same A, Same D); Items [5]]); 
(E, [Common (Same C, Common (Same A, Common (Same D, Same B)))]) 
]) = [(A, [2; 4; 8]); (B, [3; 4; 6; 8; 9]); (C, [2; 3; 4; 5; 6; 8; 9; 10]); (D, [5; 10]); (E, [])]); 

(* invalid input
print_endline "11";
print_bool ((shoppingList [ 
(A, [Items [1;2;3;1;2;3]]); 
(D, [Items [5;5;5;5;5]]); 
(A, [Same D]); 
(E, [Except (Items [1;2;3;1;2;3], [1;2;3])]); 
(A, [Items [1;2;3;4]]); 
]) = [(A, [1; 2; 3; 4; 5]); (B, []); (C, []); (D, [5]); (E, [])]); 
let x = shoppingList [ 
(A, [Items [1;2;3;1;2;3]]); 
(D, [Items [5;5;5;5;5]]); 
(A, [Same D]); 
(E, [Except (Items [1;2;3;1;2;3], [1;2;3])]); 
(A, [Items [1;2;3;4]]); 
] in
print_state x;
*)
print_endline "pass all tests";

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
