use "interpreter/values.sml";

exception UnboundVariable of string
exception VariableNotLambda of string

fun find (var, l : (char * 'a) list) =
  	case l of 
  		 [] => raise UnboundVariable (Char.toString var ^ " never declared in main function")
     	| t :: l' => 
     		if (#1 t) = var
     		then (#2 t)
            else find(var, l')

fun getval v = 
	case v of 
		 (Integer k) => k
		 | (Boolean b) => raise VariableNotLambda "Sum of integer and boolean not possible"

fun getbool v =
	case v of 
		  (1) => Boolean true
		 |(0) => Boolean false 

fun getvalclos c = 
	case c of
		(Closure c) => c
		| _ => raise VariableNotLambda "A variable in the program is not a function"

(*Funzione di valutazione delle espressioni*)
fun eval_exp(env, Cons(k)) = Integer k
	| eval_exp(env, BoolCons(k)) = getbool(k)
	| eval_exp(env, VarExp(Var(v))) = find(v, env)
	| eval_exp(env, Plus(e1,e2)) = 
		let
			val op1 = getval(eval_exp(env, e1))
			val op2 = getval(eval_exp(env, e2))
		in
			Integer(op1 + op2)
		end
	| eval_exp(env, Apply(Var(v), args)) = 
		let
			val c = getvalclos(find(v, env)) 		(*Prende il valore della chiusura dall'ambiente*)
			val vl = #1 c 							(*lista dei parametri*)
			val body = #2 c 						(*corpo della lambda*)
			val clos_env = #3 c 					(*Ambiente in cui valutare il corpo*)
			
			fun eval_args e = eval_exp(env, e)		(*Funzione wrapper a eval_exp, puo essere mappata ad una lista di espressioni*)
			
			val interpreted_args = List.map eval_args args

			(* Funzione utilizzata per aumentare l'ambiente env
				input: una lista di variabili l e una lista di valori el
				output: una lista di coppie (l[i], el[i]) per ogni i.
				Serve per associare ad ogni parametro formale della funzione, il valore degli arogmenti
			*)
			fun add_to_env (vl : char list, el) =
				case vl of
					  [] => []
					| (x :: vl') =>
						let
							val e1 = List.hd el
							val el = List.drop (el,1)
						in 
							(x, e1) :: add_to_env(vl',el)
						end
			val env = clos_env @ add_to_env(vl, interpreted_args)
		in
			eval_exp(env, body)
		end
	| eval_exp(env, Lambda(vl, e)) = 
		let 
			(* Funzione per estrarre le variabili di input di una lambda
				input: un oggetto di tipo Variable. 
				output: il carattere associato alla variabile
			*)
			fun take_char l = case l of (Var v) => v
			
			val params = List.map take_char vl
		in
			Closure((params, e, env))
		end

fun eval(Prog(dl, t, Var(v), e1, e2)) = 
	let
		val env = []
		val v1 = eval_exp(env, e1)
		val env = (v, v1) :: env
		val v2 = eval_exp(env, e2)
	in
		v2
	end