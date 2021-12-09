use "tokenizer.sml";
use "interface_parser.sml";
use "expression_parser.sml";

Control.Print.printLength := 500;
Control.Print.printDepth := 500;

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
		| _ => raise SyntaxError "Bad Input"

fun parse ts =
	let
		val (my_list, ts') = parse_interfaces ts []
		
		val (main_type, main_var, ts'') = parse_main_declaration ts'

		val (exp1, ts''') = parse_expression ts'' []
		val (exp2, ts'''') = parse_expression ts''' []
	in 
		case ts'''' of
			nil => Prog(my_list, main_type, main_var, exp1, exp2)
			| _ => raise SyntaxError "Bad Input."
	end

fun readlist (infile : string) =  
  let
      val ins = TextIO.openIn infile 
      fun loop indata = 
          case TextIO.inputLine indata of 
              SOME line => line ^ loop indata 
            | NONE      => ""
      val result = loop ins
  in 
     TextIO.closeIn ins;
     result
  end 

val P = readlist "programma.txt";
val p = parse(tokenize (String.tokens delimitator (String.translate replace P)));
