Control.Print.printLength := 500;
Control.Print.printDepth := 500;

fun foo ts = 
	case ts of
		(TokenCons s :: ts') => (Cons(valOf (Int.fromString s)), ts')
		|(TokenVar v :: ts') => (VarExp(Var v), ts')
		| _ => (VarExp(Var #"c"), ts)

fun lambda_var ts var_list=
	case ts of 
		(TokenVar v :: ts') =>
			let
				val var_list = Var v :: var_list
			in
				lambda_var ts' var_list
			end
		| _ => (var_list, ts)

fun parse_expression ts var =
	let
		val (e, ts') = foo ts
	in
		case ts' of
			(TokenPlus :: ts'') =>
				let
					val (e', ts''') = parse_expression ts'' var 
				in
					(Plus(e, e'), ts''')
				end
			|(TokenLambda :: ts'') =>
				let
					val (e', ts''') = parse_expression ts'' var 
				in 
					(Lambda(var, e'), ts''')
				end
			|(TokenVar v :: ts'') =>
				let 
					val (var, ts''') = lambda_var ts var
				in 
					(e, ts''')
				end
			| _ => (e, nil)
	end