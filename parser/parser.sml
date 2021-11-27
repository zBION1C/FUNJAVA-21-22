use "tokenizer.sml";
use "interface_parser.sml";

Control.Print.printLength := 500;
Control.Print.printDepth := 500;

val P = "interface I {int m (int x);} interface J {int m (I x, I y, int z);}"; (*class P {public static void main(String[] args) {J w = (x, y, z) -> x.m(y.m(z));System.out.println(w.m((u)-> u+1, (v) -> 41, 8)); } }";*)

tokenize (String.tokens delimitator (String.translate replace P));

exception SyntaxError of string
fun parse ts interfaceList =
	case ts of
		(TokenInterface c :: ts') => 
		let
			val (i, ts') = parse_interface ts c;
			val interfaceList = i :: interfaceList;
		in
			parse ts' interfaceList
		end
		| (_ :: ts') => raise SyntaxError "PORCODDIO";

fun parse_program ts =
	let
		val interfaceList = []
		val (p, ts') = parse ts interfaceList
	in 
		Prog(interfaceList, Int, Var #"x", Plus(Cons 1, Cons 2), Plus(Cons 1, Cons 2))
	end;



val p = parse_program(tokenize (String.tokens delimitator P));