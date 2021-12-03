use "tokenizer.sml";
use "interface_parser.sml";

Control.Print.printLength := 500;
Control.Print.printDepth := 500;

val P = "interface I {int m (int x)}; interface J {int m (I x, I y, int z);}"; (*class P {public static void main(String[] args) {J w = (x, y, z) -> x.m(y.m(z));System.out.println(w.m((u)-> u+1, (v) -> 41, 8)); } }";*)

tokenize (String.tokens delimitator (String.translate replace P));

exception SyntaxError of string

fun parse_program ts my_list = 
	case ts of
		(TokenInterface c :: ts') => 
			let 
				val (i, ts'') = parse_interface ts;
				val my_list = i :: my_list;
			in
				parse_program ts'' my_list
			end
		| nil => (my_list, nil)

fun parse ts =
	let
		val interface_list = []
		val (my_list, ts') = parse_program ts interface_list
	in 
		case ts' of
			nil => my_list
			| _ => raise SyntaxError "(*) Bad Input."
	end


val p = parse(tokenize (String.tokens delimitator (String.translate replace P)));