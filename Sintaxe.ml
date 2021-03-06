type variable = string 

type tipo = TyInt
  | TyBool
  | TyPair of tipo * tipo
  | TyFn of tipo * tipo
  | TyList of tipo
  | TyVar of int

type bop = Sub
  | Add
  | And
  | Mult 
  | Div 
  | Eq (*VER COM O SOR: equal eh só p nros ou pra true e false tbm??*)
  | Neq (* Not equal*)
  | Leq  (* less or equal*)
  | Less  
  | Geq  (* greater of equal*)
  | Greater  
  | Or 

type uop = Not

type expr = Ncte of int
  | Bcte of bool
  | Binop of bop * expr * expr
  | Unop of uop * expr
  | Pair of expr * expr 
  | If of expr *  expr *  expr
  | Var of variable
  | App of expr * expr
  | Lam of variable * expr
  | Let of variable * expr * expr
  | Lrec of variable * variable * expr * expr
  | Nil
  | Cons of expr * expr
  | IsEmpty of expr
  | Hd of expr
  | Tl of expr
  | Raise
  | Try of expr * expr

and
  env = (variable * tipo) list
and
  constrs = (tipo * tipo) list