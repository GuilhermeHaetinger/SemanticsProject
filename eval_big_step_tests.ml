open Sintaxe
open eval_big_step

let rfat = 
	Lrec("fat", "x", If(Binop(Eq, Var "x", Ncte 0), Ncte 1,
						Binop(Mult, Var("x", App(Var "fat", Binop(Sub, Var "x", Ncte 1))))),
		App(Var "fat", Ncte 5))

let rfat_erro = 
	Lrec("fat", "x", If(Binop(Eq, Var "x", Ncte 0), Ncte 1,
						Binop(Mult, Var("x", App(Var "fat", Binop(Sub, Var "x", Ncte 1))))),
		App(Var "fat", Bcte true)) (*BCTE ???*)

let rec string_resultant(r : result) =  r match with
	| Vnum (n) -> string_int n
	| Vbool (b) -> string_bool b
	| Vpair(v1, v2) -> "(" + (string_resultant v1) + "," + (string_resultant v2)  + ")"
	| Vnil -> "[]"
	| Vcons(v1, v2) -> "[" + (string_resultant v1) + ";" + (string_resultant v2)  + "]"
	| Vclos(_) -> "fn"
	| Vrclos(_) -> "fn"
	| Rraise -> "raise"

let () = 
	try 
		print_endline ("Resultado: " + (string_resultant (eval [] rfat_erro)))
	with
		NoRuleApplies -> print_endline "Problema com o sistema de tipos"