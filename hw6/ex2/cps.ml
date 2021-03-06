(*
 * SNU 4190.310 Programming Languages 
 * Homework "Continuation Passing Style" Skeleton
 *)

open M0

let count = ref 0

let new_name () = 
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let rec alpha_conv exp subst = 
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subst) with Not_found -> Var x)
  | Fn (x, e) ->
    let x' = new_name () in
    let subst' = (x, x') :: subst in
    Fn (x', alpha_conv e subst')
  | App (e1, e2) -> App (alpha_conv e1 subst, alpha_conv e2 subst)
  | Rec (f, x, e) -> 
    let x' = new_name () in
    let f' = new_name () in
    let subst' = (f, f') :: (x, x') :: subst in
    Rec (f', x', alpha_conv e subst')
  | Ifz (e1, e2, e3) -> 
    Ifz (alpha_conv e1 subst, alpha_conv e2 subst, alpha_conv e3 subst)
  | Add (e1, e2) -> Add (alpha_conv e1 subst, alpha_conv e2 subst)
  | Pair (e1, e2) -> Pair (alpha_conv e1 subst, alpha_conv e2 subst)
  | Fst e -> Fst (alpha_conv e subst)
  | Snd e -> Snd (alpha_conv e subst)

let rec cps' exp = 
  let k = new_name () in
  match exp with
  (* Constant expressions *)
  | Num n -> Fn (k, App (Var k, Num n) )
  | Var x -> Fn (k, App (Var k, Var x) )
  | Fn (x, e) ->
      let k' = new_name () in
      Fn (k, App (Var k, Fn (k', Fn (x, App (cps' e, Var k')))))
  | Rec (f, x, e) ->
      let k' = new_name () in
      Fn (k, App (Var k, Rec (f, k', Fn (x, App (cps' e, Var k')))))
  (* Non constant expressions *)
  | App (e1, e2) -> 
      let f = new_name () in
      let v = new_name () in
      Fn (k, 
          App (cps' e2,
              Fn (f,
                  App(cps' e1, 
                      Fn (v, 
                          App ( App (Var v, Var k), Var f )
                          )
                      )
                  )
              )
          )
  | Ifz (e1, e2, e3) ->
      let v1 = new_name () in
      let v2 = new_name () in
      let v3 = new_name () in
      Fn (k, 
          App (cps' e1,
              Fn (v1,
                  Ifz (Var v1,
                      App (cps' e2,
                          Fn (v2, App (Var k, Var v2)))
                      ,
                      App (cps' e3,
                          Fn (v3, App (Var k, Var v3)))
                  )
              )
          )
      )
  | Add (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k, 
        App (cps' e1, 
            Fn (v1, 
                App (cps' e2, 
                    Fn (v2, 
                        App (Var k, Add (Var v1, Var v2))
                        )
                    )
                )
            )
        )
  | Pair (e1, e2) ->
      let v = new_name () in
      let w = new_name () in
      Fn (k, 
          App (cps' e1, 
              Fn (v,
                  App (cps' e2,
                      Fn (w, 
                          App (Var k, Pair (Var v, Var w))
                          )
                      )
                  )
              )
          )
  | Fst e ->
      let v = new_name () in
      Fn (k, 
          App (cps' e,
              Fn (v,
                  App (Var k, Fst (Var v))
                  )
              )
          )
  | Snd e ->
      let v = new_name () in
      Fn (k, 
          App (cps' e,
              Fn (v,
                  App (Var k, Snd (Var v))
                  )
              )
          )

let cps exp = cps' (alpha_conv exp [])

