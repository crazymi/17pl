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

    | K.ASSIGN (id, e) -> (trans e)@([Sm5.PUSH (Sm5.Id id); Sm5.STORE; Sm5.PUSH (Sm5.Id id); Sm5.LOAD])
    | K.SEQ (e1, e2) -> (trans e1)@[Sm5.POP]@(trans e2)
    | K.IF (e1, e2, e3) -> (trans e1)@[Sm5.JTR (trans e2, trans e3)]

    | K.WHILE (e1, e2) ->
      (* pesudo code
        while e1 do e2
        ->
        let fun e1 -> if e1 then (e2;fun e1) in
        fun e1
      *)
      let while_id = "@while_id" in
      let param = "@while_param" in
      let body = K.IF ( e1,
                        K.SEQ (e2, K.CALLV(while_id, K.NUM 1)),
                        K.UNIT) in (* else unit *)
      (trans (K.LETF(while_id, param, body, K.CALLV (while_id, K.NUM 1))))
      (* to prevent re-evaluate condition, pass arg as meaningless one 
       * before implement has error while running examples/whileread.k--
       * which has meaningless conditon evaluation *)


    | K.FOR (id, e1, e2, e3) ->
      (* pesudo code
        for i = e1 to e2 do e3
        ->
        let fun i -> if i<=e2 then (e3; fun i+1) in
        fun e1
      *)
      (*
       * iter: added new variable to save iterator
       * to prevent iter change inside for_body(e3)
       * by allocate another space, reassign iter won't effect to next iteration
       *)
      let for_id = "@for_id" in
      let iter = "@for_iter" in
      let for_end = "@for_end" in
      let body = K.IF ( K.LESS(K.VAR iter, K.ADD(K.VAR for_end, K.NUM 1)), (* if i<=e2 == if i<e2+1 *)
                        (* then e3; fun i+1 *)
                        K.SEQ (
                          K.SEQ (
                            K.ASSIGN(id, (K.VAR iter)),
                            e3
                            ),
                          K.SEQ (
                            K.ASSIGN(iter, K.ADD(K.VAR iter, K.NUM 1)),
                            K.CALLR(for_id, iter)
                          )
                        ),
                        K.UNIT) in (* else unit *)
      (trans (K.LETV(iter, e1,
                K.LETV(for_end, e2,
                  K.LETF(for_id, iter, body, K.CALLR (for_id, iter))
                )
              )
            )
      )

    | K.LETV (x, e1, e2) ->
      (trans e1) @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      (trans e2) @ [Sm5.UNBIND; Sm5.POP]
    (* note that, K--'s proc can have only a single parameter, differ from K- *)
    (* push(x,C') -> (x,C',E) *)
    | K.LETF (id, param, body, e) ->
      (* for recursive call, bind self again.
        because it'll be gone when environment change E to E' *)
      [Sm5.PUSH (Sm5.Fn (param, [Sm5.BIND id] @ (trans body) @ [Sm5.UNBIND; Sm5.POP]))] @
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
    | K.WRITE e ->
      let write_id = "@write_id" in
      (trans e) @ 
      [Sm5.MALLOC; Sm5.BIND write_id; Sm5.PUSH (Sm5.Id write_id); Sm5.STORE; Sm5.PUSH (Sm5.Id write_id); Sm5.LOAD; Sm5.PUSH (Sm5.Id write_id); Sm5.LOAD] @
      [Sm5.PUT]
    | _ -> failwith "Unimplemented"

end
