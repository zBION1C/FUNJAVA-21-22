(*
   [TokenInterface #"I",TokenIntMethod,TokenIntType,TokenVar #"x",TokenEnd,
   TokenInterface #"J",TokenIntMethod,TokenInterfaceType #"I",TokenVar #"x",
   TokenInterfaceType #"I",TokenVar #"y",TokenIntType,TokenVar #"z",TokenEnd,
   TokenInterfaceType #"J",TokenVar #"w",TokenVar #"x",TokenVar #"y",
   TokenVar #"z",TokenLambda,TokenVar #"x",TokenApply,TokenVar #"y",
   TokenApply,TokenVar #"z",TokenEnd,TokenVar #"w",TokenApply,TokenVar #"u",
   TokenLambda,TokenVar #"u",TokenPlus,TokenCons "1",TokenVar #"v",
   TokenLambda,TokenCons "41",TokenCons "8",TokenEnd]
*)

use "tokenizer.sml";
use "interface_parser.sml";
use "expression_parser.sml";

Control.Print.printLength := 500;
Control.Print.printDepth := 500;

val P = "interface I {int m (int x)}; interface J {int m (I x, I y, int z);} class P {public static void main(String[] args) {J w = (x, y, z) -> 2 + 4 + 5; System.out.println(2); } }";

tokenize (String.tokens delimitator (String.translate replace P));
 	
exception SyntaxError of string

fun parse_program ts my_list my_type my_var= 
	case ts of
		(TokenInterface c :: ts') => 
			let 
				val (i, ts'') = parse_interface ts;
				val my_list = i :: my_list;
			in
				parse_program ts'' my_list my_type my_var
			end
		| (TokenIntType :: ts') =>
			let
				val my_type = Int;
			in
				parse_program ts' my_list my_type my_var
			end
		| (TokenBoolType :: ts') =>
			let 
				val my_type = Boolean;
			in
				parse_program ts' my_list my_type my_var
			end
		| (TokenInterfaceType c :: ts') =>
			let
				val my_type = ClassInterfaceType(N c);
			in 
				parse_program ts' my_list my_type my_var
			end
		| (TokenVar v :: ts') =>
			let
				val	my_var = Var v;
			in
				parse_program ts' my_list my_type my_var
			end
		| nil => (List.rev my_list, my_type, my_var, nil)
		| _ => (List.rev my_list, my_type, my_var, ts)

fun parse ts =
	let
		val interface_list = []
		val my_type = Int
		val my_var = Var #"d"
		val (my_list, varType, myVar, ts') = parse_program ts interface_list my_type my_var
		val var = []
		val (exp1, ts'') = parse_expression ts' var

	in 
		case ts'' of
			nil => (my_list, varType, myVar, exp1)
			| _ => raise SyntaxError "Bad Input."
	end


val p = parse(tokenize (String.tokens delimitator (String.translate replace P)));