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

let add_test = (Binop(Add, Ncte(-6), Ncte(5)))
let subtract_test = (Binop(Sub, Ncte(-7), add_test))
let subtract_test_error = (Binop(Sub, Ncte(-7), Raise)) (*VER COM O SOR: ele n aceita (Binop(Sub, Ncte(-7), false)) só se coloco raise direto, ta certo isso?*)
let mult_test = (Binop(Mult, add_test, subtract_test))
let div_test_by_zero = (Binop(Div, Ncte(3), Binop(Add, Ncte(4), Ncte(-4))))
let div_not_by_zero = (Binop(Div, Ncte(0), Ncte(9)))
let test_equals_true = (Binop(Eq, Ncte(0), Ncte(0)))
let test_equals_false = (Binop(Eq, Ncte(9), Ncte(0)))
let test_equals_raise = (Binop(Eq, Raise, Ncte(0)))
let test_Not_equals_false = (Binop(Neq, Ncte(0), Ncte(0)))
let test_Not_equals_true = (Binop(Neq, Ncte(9), Ncte(0)))
let test_less_true = (Binop(Less, Ncte(0), Ncte(9)))
let test_less_false = (Binop(Less, Ncte(-8), Ncte(-8)))
let test_less_or_equal_true =  (Binop(Less, Ncte(-8), Ncte(-8)))
let test_less_or_equal_false =  (Binop(Less, Ncte(-9), Ncte(-8)))
let test_greater_false = (Binop(Less, Ncte(0), Ncte(9)))
let test_greater_true = (Binop(Less, Ncte(-8), Ncte(-8)))
let test_greater_or_equal_true =  (Binop(Less, Ncte(-8), Ncte(-8)))
let test_greater_or_equal_false =  (Binop(Less, Ncte(-9), Ncte(-8)))

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
		print_endline ("Resultado teste soma: " ^ (string_of_result (eval [] add_test)));
		print_endline ("Resultado teste subtract: " ^ (string_of_result (eval [] subtract_test)));
		print_endline ("Resultado teste erro subtração: " ^ (string_of_result (eval [] subtract_test_error)));
		print_endline ("Resultado teste mult: " ^ (string_of_result (eval [] mult_test)));
		print_endline ("Resultado teste div por zero: " ^ (string_of_result (eval [] div_test_by_zero)));
		print_endline ("Resultado teste div não por zero: " ^ (string_of_result (eval [] div_not_by_zero)));
		print_endline ("Resultado teste equals true: " ^ (string_of_result (eval [] test_equals_true)));
		print_endline ("Resultado teste equals false: " ^ (string_of_result (eval [] test_equals_false)));
		print_endline ("Resultado teste equals raise: " ^ (string_of_result (eval [] test_equals_raise)));
		print_endline ("Resultado teste not equals true: " ^ (string_of_result (eval [] test_Not_equals_true)));
		print_endline ("Resultado teste not equals false: " ^ (string_of_result (eval [] test_Not_equals_false)));
		print_endline ("Resultado teste less true: " ^ (string_of_result (eval [] test_less_true)));
		print_endline ("Resultado teste less false: " ^ (string_of_result (eval [] test_less_false)));
		print_endline ("Resultado teste less or equals false: " ^ (string_of_result (eval [] test_less_or_equal_true)));
		print_endline ("Resultado teste less or equals true: " ^ (string_of_result (eval [] test_less_or_equal_false)));		
		print_endline ("Resultado teste greater true: " ^ (string_of_result (eval [] test_greater_true)));
		print_endline ("Resultado teste greater false: " ^ (string_of_result (eval [] test_greater_false)));
		print_endline ("Resultado teste greater or equals false: " ^ (string_of_result (eval [] test_greater_or_equal_true)));
		print_endline ("Resultado teste greater or equals true: " ^ (string_of_result (eval [] test_greater_or_equal_false)));

	with
		NoRuleApplies -> print_endline "Problema com o sistema de tipos"