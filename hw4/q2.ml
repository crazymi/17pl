type require = id * (cond list)
and cond
    = Items of gift list
    | Same of id
    | Common of cond * cond
    | Except of cond * gift list
and gift = int
and id = A | B | C | D | E

(*
let shoppingList : require list -> (id * gift list) list = 
    failwith "unimplemented"
    *)
(* test code *)

let reqA : require = (A, [Items [1; 2]; Common (Same B, Same C)])
let reqB : require = (B, [Common (Same C, Items [2; 3])])
let reqC : require = (C, [Items [1]; Except (Same A, [3])])
