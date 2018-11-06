(************************************************************* Types *****************************************************************)
type tipo  = TyInt
            | TyBool
            | TyPair of tipo * tipo
            | TyFn of tipo * tipo
            | TyList of tipo
            | NewType of int

type variable = String of string 
                | Type of tipo

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
          | Var of string
          | Binop of bop * expr * expr
          | Unop of uop * expr
          | If of expr * expr * expr
          | Pair of expr * expr
          | App of expr * expr
          | Fn of variable * expr
          | Let of variable * expr * expr
          | Lrec of variable * variable * expr * expr
          | Nil
          | Cons of expr * expr
          | IsEmpty of expr
          | Hd of expr
          | Tl of expr
          | Raise
          | TryWith of expr * expr
          
type value = Vnum of int
           | Vbool of bool
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
           | Vnil
           | Vcons of value * value
           | VRaise
and
     env = (variable * tipo) list


(********************************************************* COLLECT *******************************************************************)

exception UndeclaredVar

let rec collect_constr (environment:env) (exprCounter:int) (expression:expr) = match expression with
    | Num(t) ->
        (TyInt, exprCounter, [])

    | Bool(t) ->
        (TyBool, exprCounter, [])

    | Var(name) -> (
        try (let term = (snd (List.find (fun (String(var), _) -> String.compare var name == 0) environment)) in term, exprCounter, [])
        with _ -> 
        print_endline ("\n\nvariable " ^ name ^ " not found!\n\n");
        raise UndeclaredVar
        )

    | Binop(op, t1, t2) ->
        let (typeT1, countT1, constrT1) = collect_constr environment exprCounter t1 in
            let (typeT2, countT2, constrT2) = collect_constr environment countT1 t2 in
                (match op with
                    | Eq | Neq | Leq | Less | Geq |Greater ->
                        (TyBool, countT2, List.concat [constrT1; constrT2; [(Type(typeT1), TyInt)]; [(Type(typeT2), TyInt)]])
                    | Sum | Diff | Mult | Div ->
                        (TyInt, countT2, List.concat [constrT1; constrT2; [(Type(typeT1), TyInt)]; [(Type(typeT2), TyInt)]])
                    | Or | And ->
                        (TyBool, countT2, List.concat [constrT1; constrT2; [(Type(typeT1), TyBool)]; [(Type(typeT2), TyBool)]])
                )

    | Unop(op, t) ->
        let (typeT, countT, constrT) = collect_constr environment exprCounter t in
            (TyBool, countT, List.concat [constrT; [(Type(typeT), TyBool)]])

    | Pair(t1, t2) -> 
        let (typeT1, countT1, constrT1) = collect_constr environment exprCounter t1 in
            let (typeT2, countT2, constrT2) = collect_constr environment countT1 t2 in
                (TyPair(typeT1, typeT2), countT2, List.concat [constrT1; constrT2])

    | If(t1, t2, t3) ->
        let (typeT1, countT1, constrT1) = collect_constr environment exprCounter t1 in
            let (typeT2, countT2, constrT2) = collect_constr environment countT1 t2 in
                let (typeT3, countT3, constrT3) = collect_constr environment countT2 t3 in
                    (typeT2, countT3, List.concat [constrT1; constrT2; constrT3; [(Type(typeT1), TyBool)]; [(Type(typeT2), typeT3)]])
    (* Não sei se t2 = t3 AND t3 = t2 é necessario aqui em cima *)
    | App(t1, t2) ->
        let (typeT1, countT1, constrT1) = collect_constr environment exprCounter t1 in
            let (typeT2, countT2, constrT2) = collect_constr environment countT1 t2 in
                let newType = NewType(countT2) in
                    (newType, countT2+1, List.concat [constrT1; constrT2; [(Type(typeT1), TyFn(typeT2, newType))]])

    | Fn(var, exp) ->
        let newType = NewType(exprCounter) in
            let newVar = (var, newType) in
                let (typeT, countT, constrT) = collect_constr (List.concat [environment; [newVar]]) (exprCounter+1) exp in
                    (TyFn(newType, typeT), countT, constrT)

    | Let(var, exp1, exp2) ->
        let newType = NewType(exprCounter) in
            let newVar = (var, newType) in
                let (typeT1, countT1, constrT1) = collect_constr environment (exprCounter+1) exp1 in
                    let (typeT2, countT2, constrT2) = collect_constr (List.concat [environment; [newVar]]) countT1 exp2 in
                        (typeT2, countT2, List.concat [constrT1; constrT2; [(Type(newType), typeT1)]])

    | Lrec(var1, var2, exp1, exp2) ->
        let newType1 = NewType(exprCounter) in
            let f = (var1, newType1) in
                let newType2 = NewType(exprCounter+1) in
                    let y = (var2, newType2) in
                        let (typeT1, countT1, constrT1) = collect_constr (List.concat [environment; [y]; [f]]) (exprCounter+2) exp1 in
                            let (typeT2, countT2, constrT2) = collect_constr (List.concat [environment; [f]]) countT1 exp2 in
                                (typeT2, countT2, List.concat [constrT1; constrT2; [(Type(newType1), TyFn(newType2, typeT1))]])
    
    | Nil ->
        let newType = NewType(exprCounter) in
            (TyList(newType), exprCounter+1, [])

    | Cons(exp1, exp2) ->
        let (typeT1, countT1, constrT1) = collect_constr environment exprCounter exp1 in
            let (typeT2, countT2, constrT2) = collect_constr environment countT1 exp2 in
                (typeT2, countT2, List.concat [constrT1; constrT2; [Type(TyList(typeT1)), typeT2]])

    | IsEmpty(exp) ->
        let newType = NewType(exprCounter) in
            let (typeT, countT, constrT) = collect_constr environment (exprCounter+1) exp in
                (TyBool, countT, List.concat [constrT; [(Type(typeT), TyList(newType))]])
    
    |Hd(exp) ->
        let newType = NewType(exprCounter) in
            let (typeT, countT, constrT) = collect_constr environment (exprCounter+1) exp in
                (newType, countT, List.concat [constrT; [(Type(typeT), TyList(newType))]])

    |Tl(exp) ->
        let newType = NewType(exprCounter) in
            let (typeT, countT, constrT) = collect_constr environment (exprCounter+1) exp in
                (TyList(newType), countT, List.concat [constrT; [(Type(typeT), TyList(newType))]])

    | Raise ->
        let newType = NewType(exprCounter) in
            (newType, exprCounter+1, [])
    
    |TryWith(exp1, exp2) ->
        let (typeT1, countT1, constrT1) = collect_constr environment exprCounter exp1 in
            let (typeT2, countT2, constrT2) = collect_constr environment countT1 exp2 in
                (typeT2, countT2, List.concat [constrT1; constrT2; [(Type(typeT1), typeT2)]])

;;

let collect (expre:expr) = collect_constr [] 0 expre
;;

(************************************************************** AUX ******************************************************************)

let rec tipoToString (tp:tipo) : string =
  match tp with
  | TyBool -> "bool"
  | TyInt  -> "int"
  | TyList a -> (tipoToString a) ^ " list"
  | TyFn(tp1,tp2) -> "(" ^ (tipoToString tp1) ^ " -> " ^ (tipoToString tp2) ^ ")"
  | TyPair(tp1, tp2) -> "(" ^ (tipoToString tp1) ^ ", " ^ (tipoToString tp2) ^ ")"
  | NewType(n) -> "X_" ^ string_of_int n
;;


let rec printConstraints (constr:env) =
    match constr with
    | [] ->
        print_endline (" ======== ")
    | hd::tl -> ( match hd with
        | (Type(tp1), tp2) -> 
            print_endline ("type " ^ tipoToString tp1 ^ " = type " ^ tipoToString tp2)
        | (String(str), typ) ->
            print_endline ("new var ~" ^ str ^ "~ type = " ^ tipoToString typ);
    );
    printConstraints tl;
;;

(************************************************************** UNIFY ****************************************************************)

exception Unresolvable


let substitutionInType (xId: int) (newT: tipo) (t: tipo) : tipo =
  let rec subs tyS = match tyS with
    | TyList(tyS1) -> TyList(subs tyS1)
    | TyFn(tyS1, tyS2) -> TyFn(subs tyS1, subs tyS2)
    | TyPair(tyS1, tyS2) -> TyPair(subs tyS1, subs tyS2)
    | TyInt -> TyInt
    | TyBool -> TyBool
    | NewType(n) -> if n=xId then newT else NewType(n)
  in subs t

;;

let rec substitutionInTyEquation (xId: int) (newT: tipo) (constr: env) : env = match constr with
    | (Type(t), tp)::tl -> List.concat [[(Type(substitutionInType xId newT t), substitutionInType xId newT tp)]; substitutionInTyEquation xId newT tl]
    | (tp1, tp2)::tl -> List.concat [[(tp1, substitutionInType xId newT tp2)]; substitutionInTyEquation xId newT tl]
    | [] -> []

;;

let occurCheck (xId: int) (t: tipo) =
  let rec occur typ = match typ with
    | TyList(tyT1) -> occur tyT1
    | TyFn(tyT1,tyT2) -> occur tyT1; occur tyT2
    | TyPair(tyT1,tyT2) -> occur tyT1; occur tyT2
    | TyInt -> ()
    | TyBool -> ()
    | NewType(n) ->
        if n=xId then (print_endline ((tipoToString (NewType(n))) ^ " OCCURS ON " ^ (tipoToString t) ^ " AND SETS EQ TO UNRESOLVABLE "); raise Unresolvable;)
        else ();
  in occur t

;;

let rec unify_rec (subs: env) (constr: env) =
match constr with
    | [] -> subs
    | (Type(TyInt), TyInt)::tl -> unify_rec subs tl                                                                         (* 1 *)
    | (Type(TyBool), TyBool)::tl -> unify_rec subs tl                                                                       (* 2 *)
    | (Type(TyList(t1)), TyList(t2))::tl -> unify_rec subs (List.concat [tl; [(Type(t1), t2)]])                             (* 3 *)
    | (Type(TyFn(t1, t2)), TyFn(t3, t4))::tl -> unify_rec subs (List.concat [tl; [(Type(t1), t3)]; [(Type(t2), t4)]])       (* 4 *)
    | (Type(TyPair(t1, t2)), TyPair(t3, t4))::tl -> unify_rec subs (List.concat [tl; [(Type(t1), t3)]; [(Type(t2), t4)]])   (* 5 *)                                     
    | (Type(NewType(n)), t)::tl ->
        if t=NewType(n) then unify_rec subs tl
        else(occurCheck n t; unify_rec (List.concat [subs; [(Type(NewType(n)), t)]]) (substitutionInTyEquation n t constr))
    | (Type(t), NewType(n))::tl ->                                                                                          (* 8 *)
        occurCheck n t;
        unify_rec (List.concat [subs; [(Type(NewType(n)), t)]]) (substitutionInTyEquation n t constr)
    | (_, _)::tl -> 
        print_endline ("*****************\nUNRESOLVABLE!!\n*****************");
        raise Unresolvable                                                                                                  (* 9 *)

;;

let unify (constr: env) = unify_rec [] constr

(************************************************************** APPLY ****************************************************************)

let rec applySubs (subs: env) (t: tipo) = 
    match t with 
    | TyList(tyT1) -> TyList(applySubs subs tyT1)
    | TyFn(tyT1,tyT2) -> TyFn(applySubs subs tyT1, applySubs subs tyT2)
    | TyPair(tyT1,tyT2) -> TyPair(applySubs subs tyT1, applySubs subs tyT2)
    | TyInt -> TyInt
    | TyBool -> TyBool
    | NewType(n) -> (try applySubs subs (snd (List.find (fun (Type(NewType(x)), _) -> x == n) subs))
                     with Not_found -> NewType(n))

(************************************************************ TYPEINFER **************************************************************)

let typeInfer (exp: expr) : tipo = 
    let (finalTp, _, constr) = collect exp in
        let mapSub = unify constr in
            applySubs mapSub finalTp

(************************************************************** TESTS ****************************************************************)

let rec run_constraintTests (exps: expr list) = match exps with
    | [] ->
        print_endline ("=====END OF TESTS=====");
    | hd::tl ->
        let (finalTp, _, constr) = collect hd in
            print_endline ("expression type = " ^ tipoToString finalTp);
            printConstraints constr;
            run_constraintTests tl;
;;

let rec run_typeInferTests (exps: expr list) = match exps with
    | [] ->
        print_endline ("=====END OF TESTS=====");
    | hd::tl ->
        (try
            print_endline (tipoToString (typeInfer hd));
            print_endline ("===============");
        with
        | UndeclaredVar -> ()
        | Unresolvable -> ()
        );
        run_typeInferTests tl
;;

let () = 

    (* NORMAL *)
    let t1 = Num(1) in
    let t2 = Bool(true) in
    let t3 = Binop(Sum, Num(1), Num(2)) in
    let t4 = Pair(Num(1), Bool(false)) in
    let t5 = If(Bool(true), Binop(Sum, Num(6), Num(7)), Binop(Mult, Num(7), Num(8))) in
    let t6 = App(Fn(String("var"), (Binop(Div, Num(1), Binop(Sum, Num(2), Num(1))))), Num(6)) in
    let t7 = Let(String("myVar"), Num(5), Binop(Sum, Var("myVar"), Num(5))) in
    let t8 = Lrec(String("fat"), String("x"),
                If(Binop(Eq, Var("x"), Num(0)),
                Num(1),
                Binop(Mult, Var("x"), App(Var("fat"), Binop(Diff, Var("x"), Num(1))))),
                App(Var("fat"), Num(5))) in
    let t9 = IsEmpty(Nil) in
    let t10 = Cons(Num(5), (Cons (Num(5), Nil))) in
    let t11 = Hd(Cons(Bool(true), (Cons(Bool(false), Nil)))) in
    let t12 = TryWith(Bool(true), Binop(Eq,Num(5),Num(10))) in

    (* WITH UNDEFINED TYPE *)
    let t13 = Fn(String("var"), (Binop(Div, Num(9), Binop(Sum, Num(2), Num(1))))) in
    let t14 = Nil in
    (* UNRESOLVABLE *)
    let t15 = Unop(Not, Binop(Sum, Num(1), Num(6))) in
    let t16 = Tl(Cons(Num(5), (Cons(Bool(true), Nil)))) in

    let tests = [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t13;t14;t15;t16] in

(*TYPE INFER TESTS *)
    run_typeInferTests tests

(* COLLECT TESTS *)
(* 
    try
        run_constraintTests tests
    with
        UndeclaredVar -> (); *)