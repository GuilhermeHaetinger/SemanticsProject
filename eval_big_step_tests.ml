open Sintaxe

type result = Vnum of int
			| Vbool of bool
			| Vpair of result * result
			| Vnil
			| Vcons of result * result
			| Vclos of variable * exp * env
			| Vrclos of variable * variable * exp * env
			| Rraise
and 
	env = (variable * result) list 

exception NoRuleApplies

let rec eval (env : env) (exp : expr) : result = match exp with
	  Ncte(n) -> Vnum(n)
 	| Bcte(b) -> Vbool(b)
 	| Pair(e1, e2) ->
		let v1 = eval env e1 in
			if v1 = Rraise then Rraise else
		let v2 = eval env e2 in
			if v2 = Rraise then Rraise else
				Vpair(v1, v2)
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
			 | Neq,		Vnum(v1), Vnum(v2) -> Vbool(v1 !=v2)
			 | Less, 	Vnum(v1), Vnum(v2) -> Vbool(v1 < v2)
			 | Leq, 	Vnum(v1), Vnum(v2) -> Vbool(v1 <= v2)
			 | Greater, Vnum(v1), Vnum(v2) -> Vbool(v1 > v2)
			 | Greq, 	Vnum(v1), Vnum(v2) -> Vbool(v1 >= v2)
			 | And, 	Vbool(v1), Vbool(v2) -> Vbool(v1 && v2)
			 | Or, 		Vbool(v1), Vbool(v2) -> Vbool(v1 || v2)
			 (*Se o pattern matching não aplica nenhuma regra*)
			 | _ -> raise NoRuleApplies
			 ) 
	| Unop(exp, e1) ->
		(match (eval env e1) with
			RRaise -> Rraise
		| 	Vbool(b) -> Vbool(not b)
		| _ -> raise NoRuleApplies)

	| If(e1, e2, e3) ->
		(match (eval env e1) with
			Rraise -> Rraise
		| 	Vbool(true) -> eval env e2
		| 	Vbool(false) -> eval env e3
		| _ -> raise NoRuleApplies
		)
	| Var(x) ->
		(try List.assoc x env
		with Not_found -> raise NoRuleApplies)

	| App(e1, e2) ->
		let v1 = eval env e1 in
		if v1 = Rraise then Rraise 
		else let v2 = eval env e2 in
		if v2 = Rraise then Rraise else
		(* Nenhum dos operador avalia para raise*)
		( match(v1, v2) with
        | (Vclos(x, e, env'), v) -> _eval ((x,v)::env') e
        | (Vrclos(f, x, e, env'), v) -> _eval ((x,v)::(f,Vrclos(f,x,e,env'))::env') e
		| _ -> raise NoRuleApplies)
		)

	| Hd(e1) ->
	(match (eval env e1) in
		|)

	| Tl(e1) ->
		(match (eval env e1) with
			| Rraise -> Rraise
			| VNil -> RRaise
			| Vcons(_, v) -> v 
			(*Essa exececao n é pra rolar se o typeinfer tiver top, mas vou colocar porque vai que*)
			| _ -> raise NoRuleApplies
		)
	
	| Try(e1, e2) ->
		let v1 = eval env e1 in
		if v1 = Raise then eval env e2
		else v1

	| Raise -> Raise 
