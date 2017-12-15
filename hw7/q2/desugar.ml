(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp

(* TODO : Implement this function *)
let count = ref 0
let new_name () =
  let _ = count := !count + 1 in
  "@" ^ (string_of_int !count)

let rec cps xexp = 
  let k = new_name () in
  let h = new_name () in
  let hApp (e1, e2) = 
    App (App (e1, e2), Var h)
  in
  match xexp with
  (* Constant expressions *)
  | Num n -> Fn (k, Fn (h, App (Var k, Num n) ))
  | Var x -> Fn (k, Fn (h, App (Var k, Var x) ))

  | Fn (x, e) ->
      Fn (k, Fn (h, App (Var k, Fn (x, cps e))))

  (* Non constant expressions *)
  | App (e1, e2) -> 
      let f = new_name () in
      let v = new_name () in
      Fn (k, Fn (h,
          hApp (cps e1,
              Fn (f,
                  hApp(cps e2,
                      Fn (v, 
                          hApp (App (Var f, Var v), Var k)
                          )
                      )
                  )
              )
          ))
  | If (e1, e2, e3) ->
      let v1 = new_name () in
      let v2 = new_name () in
      let v3 = new_name () in
      Fn (k, Fn (h, 
          hApp (cps e1,
              Fn (v1,
                  If (Var v1,
                      hApp (cps e2,
                          Fn (v2, App (Var k, Var v2)))
                      ,
                      hApp (cps e3,
                          Fn (v3, App (Var k, Var v3)))
                  )
              )
          )
      ))
  | Equal (e1, e2) ->
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k, Fn (h,
        hApp (cps e1, 
            Fn (v1, 
                hApp (cps e2, 
                    Fn (v2, 
                        App (Var k, Equal (Var v1, Var v2))
                        )
                    )
                )
            )
        ))
  | Handle (e1, x, e2) ->
      let v = new_name () in
      Fn (k, Fn (h,
        App (App (cps e1, Var k),
          Fn (v,
            If (Equal (Var v, Num x),
              hApp (cps e2, Var k),
              App (Var h, Var v)
            )
          )
        )
      ))
  | Raise e -> Fn(k, Fn (h, hApp (cps e, Var h)))

let removeExn : xexp -> xexp = fun e ->
  let k = new_name () in
  let h = new_name () in
  App (App (cps e, Fn(k, Var k)), Fn(h, Num 201712))
