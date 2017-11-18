(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR id -> [Sm5.PUSH (Sm5.Id id); Sm5.LOAD]

    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> (trans e1)@(trans e2)@([Sm5.SUB])
    | K.MUL (e1, e2) -> (trans e1)@(trans e2)@([Sm5.MUL])
    | K.DIV (e1, e2) -> (trans e1)@(trans e2)@([Sm5.DIV])

    | K.EQUAL (e1, e2) -> (trans e1)@(trans e2)@([Sm5.EQ])
    | K.LESS (e1, e2) -> (trans e1)@(trans e2)@([Sm5.LESS])
    | K.NOT e -> (trans e)@([Sm5.NOT])

    | K.ASSIGN (id, e) -> (trans e)@([Sm5.PUSH (Sm5.Id id); Sm5.STORE])
    | K.SEQ (e1, e2) -> (trans e1)@(trans e2)
    | K.IF (e1, e2, e3) -> (trans e1)@[Sm5.JTR (trans e2, trans e3)]

    | K.WHILE (e1, e2) -> failwith "while unimplemented"
    | K.FOR (id, e1, e2, e3) ->
      (* pesudo code
        for i = e1 to e2 do e3
        ->
        let fun i -> if i<=e2 then (e3; fun i+1) in
        fun e1
      i*)
      let for_id = "for_id" in
      let param = id in
      let body = K.IF ( K.LESS(K.VAR id, K.ADD(e2, K.NUM 1)), (* if i<=e2 *)
                        (* then e3; fun i+1 *)
                        K.SEQ (e3, K.CALLV(for_id, K.ADD(K.VAR id, K.NUM 1))),
                        K.UNIT) in (* else unit *)
      (trans (K.ASSIGN (id, e1))) @ 
      (trans (K.LETF(for_id, param, body, K.CALLR (for_id, id))))

    | K.LETV (x, e1, e2) ->
      (trans e1) @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      (trans e2) @ [Sm5.UNBIND; Sm5.POP]
    (* note that, K--'s proc can have only a single parameter, differ from K- *)
    (* push(x,C') -> (x,C',E) *)
    | K.LETF (id, param, body, e) ->
      (* for recursive call, bind self again.
        because it'll be gone when environment change E to E' *)
      [Sm5.PUSH (Sm5.Fn (param, [Sm5.BIND id] @ (trans body)))] @
      [Sm5.BIND id] @
      (trans e) @ [Sm5.UNBIND; Sm5.POP]

    | K.CALLV (id, e) ->
      (* l::v::(x,C',E') *)
      [Sm5.PUSH (Sm5.Id id)] @
      (* above line is for recursive call, to handle line at LETF's [Sm5.BIND id] @ ~ *)
      [Sm5.PUSH (Sm5.Id id)] @ (* get proc from env  = (x,C',E') *)
      (trans e) @ (* push parameter = v *)
      [Sm5.MALLOC; Sm5.CALL] (* alloc for param and call = l *)
      
    | K.CALLR (id, x) ->
      [Sm5.PUSH (Sm5.Id id)] @
      [Sm5.PUSH (Sm5.Id id)] @
      [Sm5.PUSH (Sm5.Id x); Sm5.LOAD] @
      [Sm5.PUSH (Sm5.Id x)] @ (* pass pointer *)
      [Sm5.CALL]

    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> (trans e)@([Sm5.PUT])
    | _ -> failwith "Unimplemented"

end
