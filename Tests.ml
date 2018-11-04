open Sintaxe
open Eval

(* PARA RODAR : ocamlopt -o prog Sintaxe.ml Eval.ml Tests.ml e dps * / prog *)
 
let add_test = (Binop(Add, Ncte(-6), Ncte(5)))
let subtract_test = (Binop(Sub, Ncte(-7), add_test))
let subtract_test_error = (Binop(Sub, Ncte(-7), Raise)) 
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
let test_greater_false = (Binop(Greater, Ncte(0), Ncte(9)))
let test_greater_true = (Binop(Greater, Ncte(8), Ncte(-8)))
let test_greater_or_equal_true =  (Binop(Geq, Ncte(-8), Ncte(-8)))
let test_greater_or_equal_false =  (Binop(Geq, Ncte(-9), Ncte(-8)))
let test_or_true_first_term = (Binop(Or, Bcte(true), Bcte(false)))
let test_or_true_second_term = (Binop(Or,  Bcte(false), Bcte(true)))
let test_or_true_both_terms = (Binop(Or, Bcte(true), Bcte(true)))
let test_or_false_both_terms = (Binop(Or,  Bcte(false),  Bcte(false)))
let test_and_true_first_term = (Binop(And, Bcte(true), Bcte(false)))
let test_and_true_second_term = (Binop(And,  Bcte(false), Bcte(true)))
let test_and_true_both_terms = (Binop(And, Bcte(true), Bcte(true)))
let test_and_false_both_terms = (Binop(And,  Bcte(false),  Bcte(false)))
let test_not = Unop(Not, Bcte(true))
let test_not_raise = Unop(Not, Raise)
let test_lots_of_ifs = If(test_greater_true,If(Binop(Geq, Ncte(0), Ncte(0)),Ncte(50),Ncte(96)),Raise)
let test_lots_of_ifs_raise = If(test_greater_false,If(Binop(Geq, Ncte(0), Ncte(0)),Ncte(50),Ncte(96)),Raise)
let test_fatorial = 
	Lrec("fat", "x", If(Binop(Eq, Var "x", Ncte 0), Ncte 1,
						Binop(Mult, Var "x", App(Var "fat", Binop(Sub, Var "x", Ncte 1)))),
		App(Var "fat", Ncte 6))
let test_fibonacci = (Lrec("fib", "x", If(Binop(Leq, Var "x", Ncte(1)),
                    Ncte(1),
                    Binop(Add, App(Var("fib"), Binop(Sub, Var("x"), Ncte(1))) , App(Var("fib"), Binop(Sub, Var("x"), Ncte(2))))),
                App(Var("fib"), Ncte(7))))

let test_is_even_function_false = (Lrec("iseven", "variable",
					                If(Binop(Eq, Var("variable"), Ncte(1)),
					                		Bcte(false),
					                      	If(Binop(Eq, Var("variable"), Ncte(0)),
					                      		Bcte(true),
					                 			App(Var("iseven"),Binop(Sub, Var("variable"), Ncte(2))))),
                			App(Var("iseven"), Ncte(21))))  

let test_is_even_function_true = (Lrec("iseven", "variable",
					                If(Binop(Eq, Var("variable"), Ncte(1)),
					                		Bcte(false),
					                      	If(Binop(Eq, Var("variable"), Ncte(0)),
					                      		Bcte(true),
					                 			App(Var("iseven"),Binop(Sub, Var("variable"), Ncte(2))))),
                			App(Var("iseven"), Ncte(202))))

let test_cons_heterog = Cons(Binop(Add, Ncte(7), Ncte(0)), (Cons(Bcte(true), Nil)))
let test_cons_raise = Cons(Ncte(7), (Cons(Raise, Nil)))
let test_is_empty_false = IsEmpty(test_cons_heterog)
let test_is_empty_false_2 = IsEmpty(Cons(Nil,Nil))
let test_is_empty_true =  IsEmpty(Nil)
let test_head = Hd(Cons(Binop(Add, Ncte(7), Ncte(0)), Cons(Bcte(true), (Cons (Ncte(5), Nil)))))
let test_head_raise = Hd(Cons(Binop(Add, Ncte(7), Ncte(0)), Cons(Raise, (Cons (Ncte(5), Nil)))))
let test_head_raise_2 = Hd(Cons(Raise, Cons(Raise, (Cons (Ncte(5), Nil)))))
let test_tail = Tl(Cons(Binop(Add, Ncte(7), Ncte(0)), Cons(Bcte(true), (Cons (Ncte(5), Nil)))))
let test_tail_raise = Tl(Cons(Raise, Cons(Bcte(true), (Cons (Ncte(5), Nil)))))
let test_try = Try(Bcte(true), Binop(Add,Ncte(50),Ncte(11)))
let test_try_raise = Try(test_cons_raise, Binop(Add,Ncte(50),Ncte(11)))
let test_lam = Lam("une_variable", If(Binop(Eq, Var("une_variable"), Ncte(5)), Ncte(5), Ncte(5)))
let diff_if_type = If(test_greater_false,Bcte(true),Ncte(8)) (*NÃO DA ERRO, VER COM O SOR*)

let test_try_error = Try(Bcte(true), Binop(Add,Ncte(5),Ncte(10)))
let test_is_empty_error =  IsEmpty(Ncte(8))
let test_cons_erro = Cons(Ncte(7), (Cons(Ncte(7), Ncte(7))))
let rfat_erro = 
	Lrec("fat", "x", If(Binop(Eq, Var "x", Ncte 0), Ncte 1,
						Binop(Mult, Var "x", App(Var "fat", Binop(Sub, Var "x", Ncte 1)))),
		App(Var "fat", Bcte true)) 
let test_and_type_error = (Binop(And,  Ncte(0),  Bcte(false)))
let test_not_error = Unop(Not, Ncte(0))
let sist_tipos_error = (Binop(Sub, Ncte(-7), Bcte(true))) (*VER COM O SOR: dá prob c o sistema de tipos(NO rules applies), n raise, é isso??*)

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
		print_endline ("Resultado teste greater or true first term: " ^ (string_of_result (eval [] test_or_true_first_term)));	
		print_endline ("Resultado teste greater or true second term: " ^ (string_of_result (eval [] test_or_true_second_term)));
		print_endline ("Resultado teste greater or true both terms: " ^ (string_of_result (eval [] test_or_true_both_terms)));
		print_endline ("Resultado teste greater or false both terms: " ^ (string_of_result (eval [] test_or_false_both_terms)));
		print_endline ("Resultado teste not unop: " ^ (string_of_result (eval [] test_not)));
		print_endline ("Resultado teste not unop raise: " ^ (string_of_result (eval [] test_not_raise)));
		print_endline ("Resultado teste lots of ifs true: " ^ (string_of_result (eval [] test_lots_of_ifs)));
		print_endline ("Resultado teste lots of ifs raise: " ^ (string_of_result (eval [] test_lots_of_ifs_raise)));
		print_endline ("Resultado teste fatorial: " ^ (string_of_result (eval [] test_fatorial)));
		print_endline ("Resultado teste fatorial: " ^ (string_of_result (eval [] test_fibonacci)));
		print_endline ("Resultado teste é impar false: " ^ (string_of_result (eval [] test_is_even_function_false)));
		print_endline ("Resultado teste é impar true: " ^ (string_of_result (eval [] test_is_even_function_true)));
		print_endline ("Resultado teste cons heterog: " ^ (string_of_result (eval [] test_cons_heterog)));
		print_endline ("Resultado teste cons raise: " ^ (string_of_result (eval [] test_cons_raise)));
		print_endline ("Resultado teste is empty true: " ^ (string_of_result (eval [] test_is_empty_true)));
		print_endline ("Resultado teste is empty false: " ^ (string_of_result (eval [] test_is_empty_false)));
		print_endline ("Resultado teste is empty false2: " ^ (string_of_result (eval [] test_is_empty_false_2)));
		print_endline ("Resultado teste head: " ^ (string_of_result (eval [] test_head)));
		print_endline ("Resultado teste head raise: " ^ (string_of_result (eval [] test_head_raise)));
		print_endline ("Resultado teste head raise2: " ^ (string_of_result (eval [] test_head_raise_2)));
		print_endline ("Resultado teste tail: " ^ (string_of_result (eval [] test_tail)));
		print_endline ("Resultado teste tail raise: " ^ (string_of_result (eval [] test_tail_raise)));
		print_endline ("Resultado teste try: " ^ (string_of_result (eval [] test_try)));
		print_endline ("Resultado teste try raise: " ^ (string_of_result (eval [] test_try_raise)));
		print_endline ("Resultado teste lam: " ^ (string_of_result (eval [] test_lam)));
		print_endline ("Resultado teste diff_if_type: " ^ (string_of_result (eval [] diff_if_type)));

	with
		NoRuleApplies -> print_endline ("Erro no sistema de tipos")

(* Funções as quais dão erro NoRuleApplies 		
		print_endline ("Resultado teste test_try_error: " ^ (string_of_result (eval [] test_try_error)));
		print_endline ("Resultado teste test_cons_erro: " ^ (string_of_result (eval [] test_cons_erro)));
		print_endline ("Resultado teste rfat_erro: " ^ (string_of_result (eval [] rfat_erro)));
		print_endline ("Resultado teste test_and_type_error: " ^ (string_of_result (eval [] test_and_type_error)));
		print_endline ("Resultado teste test_not_error: " ^ (string_of_result (eval [] test_not_error)));
		print_endline ("Resultado teste sist_tipos_error: " ^ (string_of_result (eval [] sist_tipos_error))); *)