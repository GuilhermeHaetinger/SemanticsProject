type variable = String of string 
                | Int of int

type tipo  = TyInt
            | TyBool
            | TyPair of tipo * tipo
            | TyFn of tipo * tipo
            | TyList of tipo
            | TyId of int
            | NewType of int

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
          | Binop of bop * expr * expr
          | Unop of uop * expr
          | If of expr * expr * expr
          | Var of variable
          | Pair of expr * expr
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

let rec collect_constr (environment:env) (exprCounter:int) (typeCounter:int) (expression:expr) = match expression with
    | Num(t) ->
        (exprCounter, typeCounter, [])
    | Bool(t) ->
        (exprCounter, typeCounter, [])
    | Binop(op, t1, t2) ->
        let (countT1, typeCounterT1, constrT1) = collect_constr environment exprCounter typeCounter t1 in
        let (countT2, typeCounterT2, constrT2) = collect_constr environment countT1 typeCounterT1 t2 in
                (match op with
                    | Eq | Neq | Leq | Less | Geq |Greater | Sum | Diff | Mult | Div ->
                        (countT2+2, typeCounterT2, List.concat [constrT1; constrT2; [(Int(countT2+1), TyInt)]; [(Int(countT2+2), TyInt)]])
                    | Or | And ->
                        (countT2+2, typeCounterT2, List.concat [constrT1; constrT2; [(Int(countT2+1), TyBool)]; [(Int(countT2+2), TyBool)]])
                )
    | Unop(op, t) ->
        let (countT, typeCounterT, constrT) = collect_constr environment exprCounter typeCounter t in
            (countT+1, typeCounterT, List.concat [constrT; [(Int(countT), TyBool)]])
    | Pair(t1, t2) -> 
        let (countT1, typeCounterT1, constrT1) = collect_constr environment exprCounter typeCounter t1 in
            let (countT2, typeCounterT2, constrT2) = collect_constr environment countT1 typeCounterT1 t2 in
                (countT2, typeCounterT2, List.concat [constrT1; constrT2])
    | If(t1, t2, t3) ->
        let (countT1, typeCounterT1, constrT1) = collect_constr environment exprCounter typeCounter t1 in
            let (countT2, typeCounterT2, constrT2) = collect_constr environment countT1 typeCounterT1 t2 in
                let (countT3, typeCounterT3, constrT3) = collect_constr environment countT2 typeCounterT2 t3 in
                    (countT3+3, typeCounterT3, List.concat [constrT1; constrT2; constrT3; [(Int(countT1), TyBool)]; [(Int(countT2), TyId(countT3))]; [(Int(countT3), TyId(countT2))]])
    | App(t1, t2) ->
        let (countT1, typeCounterT1, constrT1) = collect_constr environment exprCounter typeCounter t1 in
            let (countT2, typeCounterT2, constrT2) = collect_constr environment countT1 typeCounterT1 t2 in
                let newType = NewType(typeCounterT2) in
                    (countT2+1, typeCounterT2+1, List.concat [constrT1; constrT2; [(Int(countT2), TyFn(TyId(countT2), newType))]])
    | Lam(var, typ, exp) ->
        let newVar = (var, typ) in
            let (countT, typeCounterT, constrT) = collect_constr (List.concat [environment; [newVar]]) exprCounter typeCounter exp in
                (countT, typeCounterT, List.concat [constrT; [newVar]])


let cas (expre:expression) = collect_constr [] 0 0 expre

let (count, typecount, contr) = cas Num(1)
(*        print_endline ("5 + 10 --> " ^ string_of_int count ^ "   " ^ string_of_int typecount)*)