use "parser/tokenizer.sml";
use "parser/interface_parser.sml";
use "parser/expression_parser.sml";

exception SyntaxError of string
fun parse_interfaces ts my_list = 
	case ts of
		(TokenInterface c :: ts') => 
			let 
				val (i, ts'') = parse_interface ts;
				val my_list = i :: my_list;
			in
				parse_interfaces ts'' my_list
			end
		| _ => (List.rev my_list, ts)

fun parse_main_declaration ts = 
	case ts of
		(TokenIntType :: TokenVar v :: ts') => (Int, Var v, ts')
		| (TokenBoolType :: TokenVar v :: ts') => (Boolean, Var v, ts')
		| (TokenInterfaceType c :: TokenVar v :: ts') => (ClassInterfaceType(N c), Var v, ts')
		| _ => raise SyntaxError "Syntax Error, check the program for errors."

fun parse ts =
	let
		val (my_list, ts') = parse_interfaces ts []
		
		val (main_type, main_var, ts'') = parse_main_declaration ts'

		val (exp1, ts''') = parse_expression(ts'')
		val (exp2, ts'''') = parse_expression(ts''')
	in 
		case ts'''' of
			 nil => Prog(my_list, main_type, main_var, exp1, exp2)
			|(RPAREN :: TokenEnd :: []) =>  Prog(my_list, main_type, main_var, exp1, exp2)
			| _ => raise SyntaxError "Syntax Error, check the program for errors."
	end