open Sintaxe

(********************************************************* COLLECT ***********)

exception UndeclaredVar of string

let rec collect_ (envi:env) (count:int) (exp:expr): tipo*int*constrs = 
  match exp with
  | Ncte(t) ->
    (TyInt, count, [])

  | Bcte(t) ->
    (TyBool, count, [])

  | Var(name) -> (
    try (let term =
      (snd (List.find (fun (var, _) -> String.compare var name == 0)
        (List.rev envi))) in term, count, [])
    with _ -> 
      raise (UndeclaredVar(name))
    )

  | Binop(op, t1, t2) ->
    let (ty1, cnt1, cnstr1) = collect_ envi count t1 in
    let (ty2, cnt2, cnstr2) = collect_ envi cnt1 t2 in
      (match op with
        | Eq | Neq | Leq | Less | Geq |Greater ->
          (TyBool,cnt2,List.concat [cnstr1;cnstr2;[(ty1,TyInt)];[(ty2,TyInt)]])
        | Add | Sub | Mult | Div ->
          (TyInt,cnt2,List.concat [cnstr1;cnstr2;[(ty1,TyInt)];[(ty2,TyInt)]])
        | Or | And ->
        (TyBool,cnt2,List.concat [cnstr1;cnstr2;[(ty1,TyBool)];[(ty2,TyBool)]])
      )

  | Unop(op, t) ->
    let (ty, cnt, cnstr) = collect_ envi count t in
      (TyBool, cnt, List.concat [cnstr; [(ty, TyBool)]])

  | Pair(t1, t2) -> 
    let (ty1, cnt1, cnstr1) = collect_ envi count t1 in
    let (ty2, cnt2, cnstr2) = collect_ envi cnt1 t2 in
      (TyPair(ty1, ty2), cnt2, List.concat [cnstr1; cnstr2])

  | If(t1, t2, t3) ->
    let (ty1, cnt1, cnstr1) = collect_ envi count t1 in
    let (ty2, cnt2, cnstr2) = collect_ envi cnt1 t2 in
    let (ty3, cnt3, cnstr3) = collect_ envi cnt2 t3 in
      (ty2,cnt3,List.concat [cnstr1;cnstr2;cnstr3;[(ty1,TyBool)];[(ty2,ty3)]])
    
  | App(t1, t2) ->
    let (ty1, cnt1, cnstr1) = collect_ envi count t1 in
    let (ty2, cnt2, cnstr2) = collect_ envi cnt1 t2 in
    let newType = TyVar(cnt2) in
      (newType,cnt2 + 1,List.concat [cnstr1;cnstr2;[(ty1,TyFn(ty2,newType))]])

  | Lam(var, exp) ->
    let newType = TyVar(count) in
    let newVar = (var, newType) in
    let (ty,cnt,cnstr) = collect_(List.concat[envi;[newVar]]) (count + 1) exp in
      (TyFn(newType, ty), cnt, cnstr)

  | Let(var, exp1, exp2) ->
    let newType = TyVar(count) in
    let newVar = (var, newType) in
    let (ty1, cnt1, cnstr1) = collect_ envi (count + 1) exp1 in
    let (ty2, cnt2, cnstr2) = collect_(List.concat[envi;[newVar]]) cnt1 exp2 in
      (ty2, cnt2, List.concat [cnstr1;cnstr2;[(newType, ty1)]])

  | Lrec(var1, var2, exp1, exp2) ->
    let newType1 = TyVar(count) in
    let f = (var1, newType1) in
    let newType2 = TyVar(count + 1) in
    let y = (var2, newType2) in
    let (ty1,cnt1,cnstr1)=collect_(List.concat[envi;[y];[f]])(count + 2)exp1 in
    let (ty2, cnt2, cnstr2) = collect_(List.concat [envi; [f]]) cnt1 exp2 in
      (ty2, cnt2, List.concat [cnstr1;cnstr2;[(newType1, TyFn(newType2,ty1))]])
    
  | Nil ->
    let newType = TyVar(count) in
      (TyList(newType), count + 1, [])

  | Cons(exp1, exp2) ->
    let (ty1, cnt1, cnstr1) = collect_ envi count exp1 in
    let (ty2, cnt2, cnstr2) = collect_ envi cnt1 exp2 in
      (ty2, cnt2, List.concat [cnstr1; cnstr2; [TyList(ty1), ty2]])

  | IsEmpty(exp) ->
    let newType = TyVar(count) in
    let (ty, cnt, cnstr) = collect_ envi (count + 1) exp in
      (TyBool, cnt, List.concat [cnstr; [(ty, TyList(newType))]])
    
  | Hd(exp) ->
    let newType = TyVar(count) in
    let (ty, cnt, cnstr) = collect_ envi (count + 1) exp in
      (newType, cnt, List.concat [cnstr; [(ty, TyList(newType))]])

  | Tl(exp) ->
    let newType = TyVar(count) in
    let (ty, cnt, cnstr) = collect_ envi (count + 1) exp in
      (TyList(newType), cnt, List.concat [cnstr; [(ty, TyList(newType))]])

  | Raise ->
    let newType = TyVar(count) in
      (newType, count + 1, [])
    
  | Try(exp1, exp2) ->
    let (ty1, cnt1, cnstr1) = collect_ envi count exp1 in
    let (ty2, cnt2, cnstr2) = collect_ envi cnt1 exp2 in
      (ty2, cnt2, List.concat [cnstr1; cnstr2; [(ty1, ty2)]])

;;

let collect (expre:expr) = collect_ [] 0 expre

;;

(************************************************************** AUX **********)

let rec tipoToString (tp:tipo) : string =
  match tp with
  | TyBool ->
    "bool"
  | TyInt  ->
    "int"
  | TyList a ->
    (tipoToString a) ^ " list"
  | TyFn(tp1,tp2) ->
    "(" ^ (tipoToString tp1) ^ " -> " ^ (tipoToString tp2) ^ ")"
  | TyPair(tp1, tp2) ->
    "(" ^ (tipoToString tp1) ^ ", " ^ (tipoToString tp2) ^ ")"
  | TyVar(n) ->
    "X_" ^ string_of_int n

;;


let rec printConstraints (constr:constrs) =
  match constr with
  | [] ->
    print_endline (" ======== ")
  | (tp1, tp2)::tl ->
    print_endline ("type " ^ tipoToString tp1 ^ " = type " ^ tipoToString tp2);
    printConstraints tl;
;;

(************************************************************** UNIFY ********)

exception Unresolvable


let substitutionInType (xId: int) (newT: tipo) (t: tipo) : tipo =
  let rec subs tyS = match tyS with
    | TyList(tyS1) -> TyList(subs tyS1)
    | TyFn(tyS1, tyS2) -> TyFn(subs tyS1, subs tyS2)
    | TyPair(tyS1, tyS2) -> TyPair(subs tyS1, subs tyS2)
    | TyInt -> TyInt
    | TyBool -> TyBool
    | TyVar(n) -> if n=xId then newT else TyVar(n)
  in subs t

;;

let rec subsInEquation (xId: int) (newT: tipo) (constr: constrs) : constrs =
  List.map (fun (tyS1,tyS2) ->
    (substitutionInType xId newT tyS1, substitutionInType xId newT tyS2))constr

;;

let occurCheck (xId: int) (t: tipo) =
  let rec occur typ = match typ with
    | TyList(tyT1) -> occur tyT1
    | TyFn(tyT1,tyT2) -> occur tyT1; occur tyT2
    | TyPair(tyT1,tyT2) -> occur tyT1; occur tyT2
    | TyInt -> ()
    | TyBool -> ()
    | TyVar(n) ->
      if n=xId
        then (
          print_endline ("X_" ^ string_of_int n ^ " OCCURS ON " ^ (tipoToString t));
          raise Unresolvable;
        )
      else ();
  in occur t

;;

let rec unify_rec (subs: constrs) (constr: constrs) =
  match constr with
  | [] -> subs
  (* 1 *)
  | (TyInt, TyInt)::tl ->
    unify_rec subs tl
  (* 2 *)
  | (TyBool, TyBool)::tl ->
    unify_rec subs tl
  (* 3 *)
  | (TyList(t1), TyList(t2))::tl ->
    unify_rec subs (List.concat [tl; [(t1, t2)]])
  (* 4 *)
  | (TyFn(t1, t2), TyFn(t3, t4))::tl ->
    unify_rec subs (List.concat [tl; [(t1, t3)]; [(t2, t4)]])
  (* 5 *)
  | (TyPair(t1, t2), TyPair(t3, t4))::tl ->
    unify_rec subs (List.concat [tl; [(t1, t3)]; [(t2, t4)]])
  | (TyVar(n), t)::tl ->
    (* 6 *)
    if t=TyVar(n) then unify_rec subs tl
    (* 7 *)
    else (
      occurCheck n t;
      unify_rec (List.concat [subs;[(TyVar(n),t)]]) (subsInEquation n t constr)
      )
  (* 8 *)
  | (t, TyVar(n))::tl ->
    occurCheck n t;
    unify_rec (List.concat [subs; [(TyVar(n), t)]]) (subsInEquation n t constr)
  | (_, _)::tl -> raise Unresolvable                                                                                                  

;;

let unify (constr: constrs) = unify_rec [] constr

(************************************************************** APPLY ********)

let rec applySubs (subs: constrs) (t: tipo) =
  match t with 
  | TyList(tyT1) -> TyList(applySubs subs tyT1)
  | TyFn(tyT1,tyT2) -> TyFn(applySubs subs tyT1, applySubs subs tyT2)
  | TyPair(tyT1,tyT2) -> TyPair(applySubs subs tyT1, applySubs subs tyT2)
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVar(n) -> (
    try applySubs subs (snd (List.find (fun (t, _) ->
      match t with
        | TyVar(x) -> x == n 
        | _ -> false
      ) subs)
    )
    with Not_found -> TyVar(n)
  )

(************************************************************ TYPEINFER ******)

let typeInfer (exp: expr) : tipo =
    let (finalTp, _, constr) = collect exp in
    let mapSub = unify constr in
      applySubs mapSub finalTp
;;