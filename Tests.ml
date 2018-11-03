open Sintaxe
open Eval

(* PARA RODAR : ocamlopt -o prog Sintaxe.ml Eval.ml Tests.ml e dps * / prog *)
let rfat = 
	Lrec("fat", "x", If(Binop(Eq, Var "x", Ncte 0), Ncte 1,
						Binop(Mult, Var "x", App(Var "fat", Binop(Sub, Var "x", Ncte 1)))),
		App(Var "fat", Ncte 5))

let rfat_erro = 
	Lrec("fat", "x", If(Binop(Eq, Var "x", Ncte 0), Ncte 1,
						Binop(Mult, Var "x", App(Var "fat", Binop(Sub, Var "x", Ncte 1)))),
		App(Var "fat", Bcte true)) 

let rec string_of_result(r : result) : string =  match r with
	| Vnum (n) -> string_of_int n
	| Vbool (b) -> string_of_bool b
	| Vpair(v1, v2) -> "(" ^ (string_of_result v1) ^ "," ^ (string_of_result v2)  ^ ")"
	| Vnil -> "[]"
	| Vcons(v1, v2) -> "[" ^ (string_of_result v1) ^ ";" ^ (string_of_result v2)  ^ "]"
	| Vclos(_) -> "fn"
	| Vrclos(_) -> "fn"
	| Rraise -> "raise"

let () = 
	try 
		print_endline ("Resultado: " ^ (string_of_result (eval [] rfat_erro)))
	with
		NoRuleApplies -> print_endline "Problema com o sistema de tipos"