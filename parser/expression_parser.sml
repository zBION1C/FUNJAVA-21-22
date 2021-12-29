fun parse_lambda ts = 
	let 
		fun parse_params(ts, arr) = 
			case ts of
				(LPAREN :: ts') => parse_params(ts', [])		
			 	|(TokenComma :: ts') => parse_params(ts', [])
			 	|(RPAREN :: ts') => ([], ts')
			 	|(TokenVar x :: ts') => 
			 		let
			 			val (var, ts'') = parse_params(ts',[])
			 			val arr = Var(x) :: var
			 		in
			 			(arr, ts'')
			 		end
		val (params, ts') = parse_params(ts, [])
	in
		case ts' of
			(TokenLambda :: ts'') =>
				let
					val (e, ts''') = parse_expression ts''
				in
					(Lambda(params, e), ts''')
				end
	end

and parse_apply ts = 
	let 
		fun take_var ts =
			case ts of
				(TokenVar x :: TokenApply :: LPAREN :: ts') => (Var(x), ts')
		val (var, ts') = take_var ts

		fun parse_args(ts,arr) = 
			let
				val (e, ts') = parse_expression ts
				val arr = e :: arr
			in
				case ts' of
					 (TokenComma :: ts'') => parse_args(ts'', arr)
					|(TokenEnd :: ts'') => (arr, ts'')
					|(RPAREN :: ts'') => (arr, ts'')
					|[] => (arr, ts')
			end
		val (el, ts'') = parse_args(ts', [])
	in 
		(Apply(var, List.rev el), ts'')
	end

and parse_atomic ts = 
	case ts of
		(TokenCons k :: ts') => (Cons(valOf (Int.fromString k)), ts')
		|(TokenBoolCons s :: ts') =>
			if (s = "false") then
				(BoolCons(0), ts')
			else
				(BoolCons(1), ts')
		|(TokenVar v :: TokenApply :: ts') =>
			let
				val (e, ts'') = parse_apply(ts)
			in 
				(e, ts'')
			end
		|(TokenVar v :: ts') => (VarExp (Var v), ts')
		|(LPAREN :: ts') => 
			let
				val (e, ts'') = parse_lambda(ts)
			in
				(e, ts'')
			end

and parse_expression ts =  
	let
		val (e, ts') = parse_atomic ts
	in
		case ts' of
			(TokenPlus :: ts'') =>
			 	let
					val (e', ts''') = parse_expression ts'' 
				in
					(Plus(e, e'), ts''')
				end
			|(RPAREN :: ts'') => (e, ts')
			|(TokenComma :: ts'') => (e, ts')
			|(TokenEnd :: ts'') => (e, ts'')
			| _ => (e, ts')
	end