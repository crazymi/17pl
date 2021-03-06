(*
 * SNU 4190.310 Programming Languages 2017 Fall
 * Type Checker Skeleton
 *)

open M
open Pp

type var = string

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)
  | TEq of var
  | TWrite of var

type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2
  
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v | TEq v | TWrite v -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TEq x' | TWrite x'
    | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

(* TODO : Implement this function *)
let rec expansive : M.exp -> bool = fun exp ->
  match exp with
  | M.CONST _ | M.VAR _ | M.FN _
  | M.READ | M.WRITE _ 
  | M.BANG _
    -> false
  | M.APP _ | M.MALLOC _ | M.ASSIGN _
    -> true
  | M.IF (e1, e2, e3)
    -> expansive(e1) || expansive(e2) || expansive(e3)
  | M.LET (M.VAL (_, e1), e2)
  | M.LET (M.REC (_, _, e1), e2)
  | M.BOP (_, e1, e2)
  | M.SEQ (e1, e2)
  | M.PAIR(e1, e2)
    -> expansive(e1) || expansive(e2)
  | M.FST e | M.SND e
    -> expansive(e)

let rec unify : typ * typ -> subst = fun tat ->
  let rec isin : var -> typ -> bool = fun alpha tau ->
    match tau with
    | TPair (t1, t2) | TFun(t1, t2) -> (isin alpha t1) || (isin alpha t2)
    | TLoc l -> isin alpha l
    | TVar v | TEq v | TWrite v -> v = alpha
    | _ -> false
  in
  match tat with
  | (t1, t2) when t1=t2 -> empty_subst
  | (TPair (t1, t2), TPair (t1', t2'))
  | (TFun (t1, t2), TFun (t1', t2')) ->
      let s = unify (t1, t1') in
      let s' = unify ((s t2), (s t2')) in
      (s' @@ s)
  | (t, TVar v)
  | (TVar v, t) ->
      if (isin v t) then
        raise (M.TypeError "unify isin fail")
      else
        make_subst v t
  | (t, TEq v)
  | (TEq v, t) ->
      if (isin v t) then
        raise (M.TypeError "unify isin fail")
      else
        begin
          match t with
          | TInt | TBool | TString | TLoc _ | TEq _ | TWrite _ -> make_subst v t
          | _ -> raise (M.TypeError "TEq type error")
        end
  | (t, TWrite v)
  | (TWrite v, t) ->
      if (isin v t) then
        raise (M.TypeError "unify isin fail")
      else
        begin
          match t with
          | TInt | TBool | TString | TWrite _ -> make_subst v t
          | _ -> raise (M.TypeError "TWrite type error")
        end
  | _ -> raise (M.TypeError "unify failure")

let rec p2s : typ -> M.typ = fun t ->
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (p2s t1, p2s t2)
  | TLoc t' -> M.TyLoc (p2s t')
  | TFun (t1, t2) -> raise (M.TypeError "fail to infer tfun type")
  | TVar v -> raise (M.TypeError ("fail to infer " ^ v ^ " type"))
  | TEq v -> raise (M.TypeError "fail to infer teq type")
  | _ -> raise (M.TypeError "fail to infer type")

(* identical to W in lecture pdf *)
let rec tcheck : typ_env * M.exp -> typ * subst = fun (env, exp) ->
  match exp with
  | M.CONST c ->
      begin
        match c with
        | M.S _ -> (TString, empty_subst)
        | M.N _ -> (TInt, empty_subst)
        | M.B _ -> (TBool, empty_subst)
      end
  | M.VAR id -> 
      begin
        try
          let (i, scheme) = List.find (fun (i', _) -> i'=id) env in
          let sub_scheme = subst_scheme empty_subst scheme in
          match sub_scheme with
          | SimpleTyp t -> (t, empty_subst)
          | GenTyp (vl, t) -> (t, empty_subst)
        with
        | Not_found -> raise (M.TypeError (id ^ " is not defined in scope"))
      end
  | M.FN (id, e) ->
      let alpha = TVar (new_var ()) in
      let alpha_scheme = (id, SimpleTyp alpha) in
      let (t, s) = tcheck (alpha_scheme :: env, e) in
      (s (TFun (s alpha, t)), s)
  | M.APP (e1, e2) ->
      let (t, s) = tcheck (env, e1) in
      let (t', s') = tcheck (subst_env s env, e2) in
      let alpha = TVar (new_var ()) in
      let s'' = unify (TFun (t', alpha), s' t) in
      ((s'' @@ s' @@ s) alpha, s'' @@ s' @@ s)
  | M.LET (decl, e2) ->
      begin
        match decl with
        | M.REC (id1, id2, e1) ->
            let alpha = TVar  (new_var ()) in
            let alpha_scheme = (id1, SimpleTyp alpha) in

            let (t, s) = tcheck (alpha_scheme :: env, M.FN (id2, e1)) in
            let s' = unify (s alpha, t) in
            let env' = subst_env (s' @@ s) env in
            let (t', s'') =
              if (expansive (M.FN (id2, e1))) then
                tcheck ((id1, SimpleTyp (s' t)) :: env', e2)
              else
                tcheck ((id1, generalize env' (s' t)) :: env', e2)
            in
            ((s'' @@ s' @@ s) t', s'' @@ s' @@ s)
        | M.VAL (id, e1) ->
            let (t, s) = tcheck (env, e1) in
            let env' = subst_env s env in
            let (t', s') = 
              if (expansive e1) then
                tcheck ((id, SimpleTyp t) :: env', e2)
              else
                tcheck ((id, generalize env' t) :: env', e2)
            in
            ((s' @@ s) t', s' @@ s)
      end
  | M.IF (e1, e2, e3) ->
      let (t, s) = tcheck (env, e1) in
      let s' = unify (t, TBool) in
      let (t', s'') = tcheck (subst_env (s' @@ s) env, e2) in
      let (t'', s''') = tcheck (subst_env (s'' @@ s' @@ s) env, e3) in
      let s'''' = unify (t', t'') in
      let slong = s'''' @@ s''' @@ s'' @@ s' @@ s in
      (slong t'', slong)
  | M.BOP (b, e1, e2) ->
      begin
        match b with
        | M.ADD | M.SUB ->
            let (t, s) = tcheck (env, e1) in
            let s' = unify (t, TInt) in
            let (t', s'') = tcheck ((subst_env (s' @@ s) env), e2) in
            let s''' = unify (t', TInt) in
            (TInt, s''' @@ s'' @@ s' @@ s)
        | M.AND | M.OR ->
            let (t, s) = tcheck (env, e1) in
            let s' = unify (t, TBool) in
            let (t', s'') = tcheck ((subst_env (s' @@ s) env), e2) in
            let s''' = unify (t', TBool) in
            (TBool, s''' @@ s'' @@ s' @@ s)
        | M.EQ ->
            let (t, s) = tcheck (env, e1) in
            let s' = unify (t, TEq (new_var ())) in
            let (t', s'') = tcheck ((subst_env (s' @@ s) env), e2) in
            let s''' = unify (t', TEq (new_var ())) in
            (TBool, s''' @@ s'' @@ s' @@ s)
      end
  | M.READ -> (TInt, empty_subst)
  | M.WRITE e ->
      let (t, s) = tcheck (env, e) in
      let s' = unify (t, TWrite (new_var ())) in
      ((s' @@ s) t, s' @@ s)
  | M.MALLOC e ->
      let (t, s) = tcheck (env, e) in
      (s (TLoc t), s)
  | M.ASSIGN (e1, e2) ->
      let (t, s) = tcheck (env, e1) in
      let (t', s') = tcheck (subst_env s env, e2) in
      begin
        match t with
        | TLoc loc ->
            let s'' = unify (loc, t') in
            ((s'' @@ s' @@ s) t', s'' @@ s' @@ s)
        | _ -> raise (M.TypeError "not location")
      end
  | M.BANG e ->
      let (t, s) = tcheck (env, e) in
      begin
        match t with
        | TLoc loc -> (s loc, s)
        | TVar v ->
            let alpha = TVar (new_var ()) in
            let s' = make_subst v (TLoc alpha) in
            ((s' @@ s) alpha, s' @@ s)
        | _ -> raise (M.TypeError "not location")
      end
  | M.SEQ (e1, e2) ->
      let (t, s) = tcheck (env, e1) in
      let (t', s') = tcheck (subst_env s env, e2) in
      ((s' @@ s) t', s' @@ s)
  | M.PAIR (e1, e2) ->
      let (t, s) = tcheck (env, e1) in
      let (t', s') = tcheck (subst_env s env, e2) in
      ((s' @@ s) (TPair (t, t')), s' @@ s)
  | M.FST e ->
      let (tpair, s) = tcheck (env, e) in
      begin
        match tpair with
        | TPair (t, _) -> (s t, s)
        | TVar v ->
            let alpha = TVar (new_var ())  in
            let alpha' = TVar (new_var ()) in
            let s' = make_subst v (TPair (alpha, alpha')) @@ s in
            (s' alpha, s')
        | _ -> raise (M.TypeError "not pair")
      end
  | M.SND e ->
      let (tpair, s) = tcheck (env, e) in
      begin
        match tpair with
        | TPair (_, t) -> (s t, s)
        | TVar v ->
            let alpha = TVar (new_var ())  in
            let alpha' = TVar (new_var ()) in
            let s' = make_subst v (TPair (alpha, alpha')) @@ s in
            (s' alpha', s')
        | _ -> raise (M.TypeError "not pair")
      end

let check : M.exp -> M.typ = fun mexp ->
  let (t, _) = tcheck ([], mexp) in
  p2s t
