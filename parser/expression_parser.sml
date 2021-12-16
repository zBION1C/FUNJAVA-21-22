exception SyntaxError of string
fun parse_base ts = 
	case ts of
		 (TokenCons s :: ts') => (Cons(valOf (Int.fromString s)), ts')
		|(TokenBoolCons s :: ts') =>
			if (s = "false") then
				(Cons(0), ts')
			else
				(Cons(1), ts')
		|(TokenVar v :: TokenComma :: ts') => (VarExp(Var v), ts')
		|(TokenVar v :: ts') => (VarExp(Var v), ts')
		|(LPAREN :: ts') => (VarExp(Var #"a"), ts')
		|(RPAREN :: ts') => (VarExp(Var #"a"), ts')
		| s => raise SyntaxError "Syntax Error, check the program for errors."

fun addVar ts =
	case ts of 
		(TokenVar v :: ts') => [Var v]
		| _ => []

fun parse_apply_expressions ts =
	let
		val exps = []
		val (e, ts') = parse_expression ts []
		val exps = e :: exps
	in
		case ts' of 
			 (TokenComma :: ts'') => 
			 	let
			 		val (loc_exps, ts''') = parse_apply_expressions ts''
			 		val exps = exps @ loc_exps
			 	in
			 		(exps, ts''')
			 	end
			| (LPAREN :: ts'') => parse_apply_expressions ts''
			| (RPAREN :: ts'') => (exps, ts'')
			| _ => (exps, ts')
	end

and parse_expression ts var =
	let
		val local_var = var
		val (e, ts') = parse_base ts

		val local_var = addVar ts @ local_var
	in
		case ts' of
			(TokenPlus :: ts'') =>
				let
					val (e', ts''') = parse_expression ts'' local_var 
				in
					(Plus(e, e'), ts''')
				end
			|(TokenLambda :: ts'') =>
				let
					val tmp = local_var
					val local_var = []
					val (e', ts''') = parse_expression ts'' local_var
				in
					(Lambda(List.rev tmp, e'), ts''')
				end
			|(TokenApply :: LPAREN :: ts'') =>
				let
					val apply_var = List.hd local_var
					val local_var = []
					val (exps, ts''') = parse_apply_expressions ts''
				in
					(Apply(apply_var, exps), ts''')
				end
			|(LPAREN :: ts'') => parse_expression ts'' local_var 
			|(RPAREN :: ts'') =>  (e, ts'')
			|(TokenComma :: ts'') => (e, ts')
			|(TokenVar v :: ts'') => parse_expression ts' local_var
			|(TokenEnd :: ts'') => (e, ts'')
			| s => raise SyntaxError "Syntax Error, check the program for errors."
	end