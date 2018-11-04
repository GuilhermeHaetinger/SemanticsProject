open Sintaxe

type result = Vnum of int
			| Vbool of bool
			| Vpair of result * result
			| Vnil
			| Vcons of result * result
			| Vclos of variable * expr * env
			| Vrclos of variable * variable * expr * env
			| Rraise
and 
	env = (variable * result) list 

exception NoRuleApplies

let rec eval (env : env) (exp : expr) : result = match exp with

	
	(* BS-NUM *)
	  Ncte(n) -> Vnum(n)

	(* BS-BOOL*)
 	| Bcte(b) -> Vbool(b)

	(* BS-PAIR OBS: acho que nao precisa pro trabalho!!*)
 	| Pair(e1, e2) ->
		let v1 = eval env e1 in
			if v1 = Rraise then Rraise else
		let v2 = eval env e2 in
			if v2 = Rraise then Rraise else
				Vpair(v1, v2)

	(* BS-BINOP *)
	| Binop(op, e1, e2) -> 
		let v1 = eval env e1 in
			if v1 = Rraise then Rraise else
				let v2 = eval env e2 in
			if v2 = Rraise then Rraise else
			(*Nenhum eh Raise*)
			(match op, v1, v2 with  
			 | Sub, 	Vnum(v1), Vnum(v2) -> Vnum(v1 - v2)
			 | Add,		Vnum(v1), Vnum(v2) -> Vnum(v1 + v2)
			 | Mult, 	Vnum(v1), Vnum(v2) -> Vnum(v1 * v2)
			 | Div, 	Vnum(v1), Vnum(v2) -> if v2 != 0 then Vnum(v1 / v2) else Rraise
			 | Eq, 		Vnum(v1), Vnum(v2) -> Vbool(v1 == v2)
			 | Neq,		Vnum(v1), Vnum(v2) -> Vbool(v1 != v2)
			 | Less, 	Vnum(v1), Vnum(v2) -> Vbool(v1 < v2)
			 | Leq, 	Vnum(v1), Vnum(v2) -> Vbool(v1 <= v2)
			 | Greater, Vnum(v1), Vnum(v2) -> Vbool(v1 > v2)
			 | Geq, 	Vnum(v1), Vnum(v2) -> Vbool(v1 >= v2)
			 | And, 	Vbool(v1), Vbool(v2) -> Vbool(v1 && v2)
			 | Or, 		Vbool(v1), Vbool(v2) -> Vbool(v1 || v2)
			 (*Se o pattern matching não aplica nenhuma regra*)
			 | _ -> raise NoRuleApplies
			 )

	(* BS-UNOP *)
	| Unop(exp, e1) ->
		(match (eval env e1) with
			Rraise -> Rraise
		| 	Vbool(b) -> Vbool(not b)
		| _ -> raise NoRuleApplies)
	
	(* BS-IF *)
	| If(e1, e2, e3) ->
		(match (eval env e1) with
			Rraise -> Rraise
		| 	Vbool(true) -> eval env e2
		| 	Vbool(false) -> eval env e3
		| _ -> raise NoRuleApplies
		)

	(* BS-ID *)
	| Var(x) ->
		(try List.assoc x env
		with Not_found -> raise NoRuleApplies)

	(* BS-APP -> OBS: DEVE SER RECONFERIDO COM O SOR*)
	| App(e1, e2) ->
		let v1 = eval env e1 in
		if v1 = Rraise then Rraise 
		else let v2 = eval env e2 in
		if v2 = Rraise then Rraise else
		(* Nenhum dos operador avalia para raise*)
		( match(v1, v2) with
        | (Vclos(x, e, env'), v) -> eval ((x,v)::env') e
        | (Vrclos(f, x, e, env'), v) -> eval ((x,v)::(f,Vrclos(f,x,e,env'))::env') e
		| _ -> raise NoRuleApplies
		)

    (* BS-FN *)
    | Lam(x, e1) -> Vclos(x, e1, env)

 	(* BS-LET *)
	| Let(x, e1, e2) ->
		(let v1 = eval env e1 in
			if v1 = Rraise then Rraise
			else eval((x, v1) :: env) e2)

	(* BS-LETREC *)
	| Lrec(f, x, e1, e2) -> 
		(let env' = (f, Vrclos(f, x, e1, env)) :: env in 
			eval env' e2)

	(* BS-NIL *)
	| Nil -> Vnil

	(* BS-CONS *)
	| Cons(e1, e2) ->
		(let v1 = eval env e1 in
			if v1 = Rraise then Rraise
		else let v2 = eval env e2 in
			(match v2 with 
				| Rraise -> Rraise
				| (Vnil | Vcons( _ , _ )) -> Vcons(v1,v2)
				| _ -> raise NoRuleApplies))

	(* BS-ISEMPTY *)
	| IsEmpty(e1) ->
		(match (eval env e1) with
			| Rraise -> Rraise
			| Vnil -> Vbool(true)
			| Vcons(_,_) -> Vbool(false)
			| _ -> raise NoRuleApplies
		)
	(* BS-HEAD *)
	| Hd(e1) ->
	(match (eval env e1) with
		| Rraise -> Rraise
		| Vnil -> Rraise
		| Vcons(v, _) -> v
		| _ -> raise NoRuleApplies
	)
 	 (* BS-TAIL *)
	| Tl(e1) ->
		(match (eval env e1) with
			| Rraise -> Rraise
			| Vnil -> Rraise
			| Vcons(_, v) -> v 
			(*Essa exececao n é pra rolar se o typeinfer tiver top, mas vou colocar porque vai que*)
			| _ -> raise NoRuleApplies
		)
	(* BS-TRYWITH *)
	| Try(e1, e2) ->
		let v1 = eval env e1 in
		if v1 = Rraise then eval env e2
		else v1
	(* BS-RAISE*)
	| Raise -> Rraise 
