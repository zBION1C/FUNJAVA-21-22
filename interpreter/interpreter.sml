datatype Values = Integer of int
				| Boolean of int
				| Closure of char list * Expression

exception UnboundVariable of char
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
		|(Boolean b) => b

fun getvalclos c = 
	case c of
		(Closure c) => c

fun eval_exp(env, Cons(k)) = Integer k
	| eval_exp(env, BoolCons(k)) = Boolean k
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
			val c = getvalclos(find(v, env))
			val vl = #1 c
			val body = #2 c
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
			val env = loop(vl, interpreted_args) @ env
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
			Closure((params, e))
		end

fun eval(Prog(dl, t, Var(v), e1, e2)) = 
	let
		val env = []
		val v1 = eval_exp(env, e1)
		val env = (v, v1) :: env
		val v2 = eval_exp(env, e2)
	in
		getval(v2)
	end