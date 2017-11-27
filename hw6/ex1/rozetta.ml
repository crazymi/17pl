(*
 * SNU 4190.310 Programming Languages 
 * Homework "Rozetta" Skeleton
 * Dongkwon Lee (dklee@ropas.snu.ac.kr)
 *)

let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z  -> Sonata.Z z
  | Sm5.B b  -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

let save x = [Sonata.MALLOC; Sonata.BIND x; Sonata.PUSH (Sonata.Id x); Sonata.STORE]
let load x = [Sonata.PUSH (Sonata.Id x); Sonata.LOAD]

let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) ->
      let addition = (load "@cont") @ [Sonata.PUSH (Sonata.Val (Sonata.B true)); Sonata.MALLOC; Sonata.CALL] in
      Sonata.Fn (arg, (
        (save "@cont") @ (* note that, Before calling, @cont is at top of stack *)
        (trans' command addition)
      ))
        (*@
        (load "@cont") @
        [Sonata.PUSH (Sonata.Val (Sonata.B true)); Sonata.MALLOC; Sonata.CALL]))
      *)

and trans' : Sm5.command -> Sonata.command -> Sonata.command = fun cmdlist add ->
  match cmdlist with
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds add)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds add)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds add)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds add)
  | Sm5.JTR (c1, c2) :: cmds -> [Sonata.JTR (trans' (c1@cmds) add, trans' (c2@cmds) add)] @ []
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds add)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds add)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds add)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds add)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds add)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds add)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds add)
  | Sm5.CALL :: cmds ->
      (save "@l") @ (save "@v") @ (save "@fn") @
      [Sonata.PUSH (Sonata.Fn ("@cont2", (trans' cmds add)))] @
      (load "@fn") @ (load "@v") @ (load "@l") @
      [Sonata.CALL]
  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds add)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds add)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds add)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds add)
  | Sm5.EQ :: cmds-> Sonata.EQ :: (trans' cmds add)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds add)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds add)
  | [] -> add

(* TODO : complete this function *)
let trans : Sm5.command -> Sonata.command = fun command ->
  trans' command []
