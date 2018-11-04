type variable = string

type tipo  = TyInt
            | TyBool
            | TyFn of tipo * tipo
            | TyList of tipo
            | TyId of string

type bop = Sum
            | Diff
            | Mult
            | Div
            | Eq
            | Neq
            | Leq
            | Less
            | Geq
            | Greater 
            | Or
            | And

type uop = Not

type expr = Num of int
          | Bool of bool
          | Binop of operator * expr * expr
          | Unop of operator * expr
          | If of expr * expr * expr
          | Var of variable
          | App of expr * expr
          | Lam of variable * tipo * expr
          | Let of variable * tipo * expr * expr
          | Lrec of variable * tipo * tipo * variable * tipo * expr * expr
          | Nil
          | Cons of expr * expr
          | IsEmpty of expr
          | Hd of expr
          | Tl of expr
          | Raise
          | TryWith of expr * expr
          | LamI of variable * expr
          | LetI of variable * expr * expr
          | LrecI of variable * variable * expr * expr

type value = Vnum of int
           | Vbool of bool
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
           | Vnil
           | Vcons of value * value
           | VRaise
and
     env = (variable * tipo) list
and
     tyeqs = (tipo * tipo) list

type nextuvar = NextUVar of string * uvargenerator
and uvargenerator = unit -> nextuvar

let uvargen =
  let rec f n () = NextUVar("?X_" ^ string_of_int n, f (n+1))
  in f 0

exception UndeclaredVar


let rec collect_constr (environment:env) (exprCounter:int) (expression:expr) =
    match expression with

    | Num(t) ->
        (exprCounter+1, [])
    | Bool(t) ->
        (exprCounter+1, [])
    | Binop(op, t1, t2) ->
        let (countT1, constrT1) = collect_constr environment exprCounter t1 in
            let (countT2, constrT2) = collect_constr environment countT1 t2 in
                (match op with
                    | Eq | Neq | Leq | Less | Geq |Greater | Sum | Diff | Mult | Div ->
                        (countT2+2, List.concat [constrT1; constrT2; (countT2+1, TyInt); (countT2+2, TyInt)])
                    | Or | And ->
                        (countT2+2, List.concat [constrT1; constrT2; (countT2+1, TyBool); (countT2+2, TyBool)])
                )
    | Unop(op, t) ->
        let (countT, constrT) = collect_constr exprCounter environment t in
            (countT+1, List.concat [constrT; (t, TyBool)])
    |