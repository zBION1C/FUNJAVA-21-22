use "tokenizer.sml";
Control.Print.printLength := 500;

exception SyntaxError of string
fun parse_method ts x y z= 
	case ts of
		(TokenIntMethod :: ts') => let val x = Int in parse_method ts' x y z end
		| (TokenBoolMethod :: ts') => let val x = Boolean in parse_method ts' x y z end
		| (TokenIntType :: TokenVar v :: ts') => let val y = Int :: y; val z = Var v :: z in parse_method ts' x y z end
		| (TokenBoolType :: TokenVar v :: ts') => let val y = Boolean :: y; val z = Var v :: z in parse_method ts' x y z end
		| (TokenInterfaceType i :: TokenVar v :: ts') => let val y = ClassInterfaceType(N i) :: y; val z = Var v :: z in parse_method ts' x y z end
		| (TokenEnd :: ts') => (x,List.rev y,List.rev z, ts')
		| (_ :: ts') => raise SyntaxError "Errore di sintassi";

fun parse_interface ts c= 
	let 
		val x = Int
		val y = []
		val z = []
		val (x,y,z,ts') = parse_method ts x y z
	in
		(Interface(N c,x,y,z), ts')
	end;