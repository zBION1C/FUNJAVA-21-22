exception UnboundVariable of char
fun find (var, l : (char * 'a) list) =
  	case l of 
  		 [] => raise UnboundVariable var
     	| t :: l' => 
     		if (#1 t) = var
     		then (#2 t)
            else find(var, l')

fun eval_exp(Cons(k), env_var, env_fun) = SOME(k)
	| eval_exp(BoolCons(k), env_var, env_fun) = SOME(k)
	| eval_exp(VarExp(Var(v)), env_var, env_fun) = SOME(find(v, env_var))
	| eval_exp(Plus(e1,e2), env_var, env_fun) = 
		let
			val sum = valOf(eval_exp(e1, env_var, env_fun)) + valOf(eval_exp(e2, env_var, env_fun))
		in
			SOME(sum)
		end
	| eval_exp(Lambda(vl, e), env_var, env_fun) = NONE


fun eval_lambda(Lambda(vl, e)) = (vl, e)

fun eval(Prog(dl, t, Var(v), e1, e2)) = 
	let
		val env_var = []
		val env_fun = []
		val v1 = eval_exp(e1,env_var, env_fun)
	in
		case v1 of
			SOME k => 
				let 
					val v1 = valOf(v1)
					val env_var = (v, v1) :: env_var
					val v2 = valOf(eval_exp(e2, env_var, env_fun))
				in
					v2
				end
			| NONE =>
				let 
					val v1 = eval_lambda(e1)
					val env_fun = (v, v1) :: env_fun
					val v2 = valOf(eval_exp(e2, env_var, env_fun))
				in
					v2
				end
	end