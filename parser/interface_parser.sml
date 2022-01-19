(* Funzione che parsa la dichiarazione di un interfaccia: nome, tipo di ritorno, parametri e rispettivi tipi *)
fun parse_method ts name x y z= 
	case ts of
		  (TokenIntMethod :: ts') => let val x = Int in parse_method ts' name x y z end
		| (TokenBoolMethod :: ts') => let val x = Boolean in parse_method ts' name x y z end
		| (TokenInterfaceMethod i :: ts') => let val x = ClassInterfaceType(N i) in parse_method ts' name x y z end
		| (TokenIntType :: TokenVar v :: ts') => let val y = Int :: y; val z = Var v :: z in parse_method ts' name x y z end
		| (TokenBoolType :: TokenVar v :: ts') => let val y = Boolean :: y; val z = Var v :: z in parse_method ts' name x y z end
		| (TokenInterfaceType i :: TokenVar v :: ts') => let val y = ClassInterfaceType(N i) :: y; val z = Var v :: z in parse_method ts' name x y z end
		| (TokenInterface c :: ts') => let val name = c in parse_method ts' name x y z end 
		| (TokenComma :: ts') => parse_method ts' name x y z
		| (LPAREN :: ts') => parse_method ts' name x y z
		| (RPAREN :: ts') => parse_method ts' name x y z	
		| (TokenEnd :: ts') => (name, x,List.rev y,List.rev z, ts')

fun parse_interface ts= 
	let 
		val name = #"c" 	(* Nome dell'interfaccia *)
		val x = Int 		(* Tipo di ritorno del metodo *)
		val y = [] 			(* Lista dei tipi dei parametri *)
		val z = [] 			(* Lista dei parametri *)
		val (name,x,y,z,ts') = parse_method ts name Int [] []
	in
		(Interface(N name,x,y,z), ts')
	end