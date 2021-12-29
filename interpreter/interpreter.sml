datatype Values = Integer of int
				| Boolean of bool
				| Closure of char list * Expression * (char * Values) list

exception UnboundVariable of char
exception VariableNotLambda of string
fun find (var, l : (char * 'a) list) =
  	case l of 
  		 [] => raise UnboundVariable var
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
			val vl = #1 c 							(*lista delle parametri*)
			val body = #2 c 						(*corpo della lambda*)
			val clos_env = #3 c 					(*Ambiente in cui valutare il corpo*)
			fun f e = eval_exp(env, e)
			val interpreted_args = List.map f args
			fun loop (l : char list ,el) =
				case l of
					  [] => []
					| (x :: l') =>
						let
							val e1 = List.hd el
							val el = List.drop (el,1)
						in 
							(x, e1) :: loop(l',el)
						end
			val env = clos_env @ loop(vl, interpreted_args)
		in
			eval_exp(env, body)
		end
	| eval_exp(env, Lambda(vl, e)) = 
		let 
			fun g l = 
				case l of
					(Var v) => v
			val params = List.map g vl
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