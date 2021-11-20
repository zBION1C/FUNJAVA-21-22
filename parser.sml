datatype Variable = Var of string;
datatype Name =  N of string;
datatype Type = ClassInterfaceType of Name | Int | Boolean;
datatype Expression = Cons of int | BoolCons of bool | VarExp of Variable | Plus of Expression * Expression | Lambda of Variable list * Expression | Apply of Variable * Expression list
datatype Declaration = Interface of Name * Type * Type list * Variable list;
datatype Program = Prog of Declaration list * Name * Type * Variable * Expression * Expression;

datatype token = 
	TokenInterface of string | TokenIntVar of string | TokenBoolVar of string | TokenInterfaceVar of string * string | TokenIntMethod | TokenBoolMethod

fun tokenize nil = nil 
	| tokenize ("interface" :: name :: r) = TokenInterface name :: tokenize r 
	| tokenize ("int" :: "m" :: r) = TokenIntMethod :: tokenize r
	| tokenize ("boolean" :: "m" :: r) = TokenBoolMethod :: tokenize r
	| tokenize ("int" :: v :: r) = TokenIntVar v :: tokenize r
	| tokenize ("boolean" :: v :: r) = TokenBoolVar	v :: tokenize r
	| tokenize (i :: v :: r) = TokenInterfaceVar(i,v) :: tokenize r;

fun delimitator c = c = #" " 
		orelse 	c = #"," 
		orelse 	c = #"{" 
		orelse  c = #"}"
		orelse 	c = #"(" 
		orelse  c = #")"
		orelse 	c = #";";

val I = "interface J { int m ( I x, I y, int z ) ; }";
tokenize (String.tokens delimitator I);

fun parse_method ts x y z= 
	case ts of
		(TokenIntMethod :: ts') => let val x = Int in parse_method ts' x y z end
		| (TokenBoolMethod :: ts') => let val x = Boolean in parse_method ts' x y z end
		| (TokenIntVar v :: ts') => let val y = Int :: y; val z = Var v :: z in parse_method ts' x y z end
		| (TokenBoolVar v :: ts') => let val y = Boolean :: y; val z = Var v :: z in parse_method ts' x y z end
		| (TokenInterfaceVar(i,v) :: ts') => let val y = ClassInterfaceType(N i) :: y; val z = Var v :: z in parse_method ts' x y z end
		| _ => (x,List.rev y,List.rev z, ts)

fun parse_interface ts = 
	case ts of
		(TokenInterface c :: ts') => 
		let 
			val x = Int
			val y = []
			val z = []
			val (x,y,z,ts'') = parse_method ts' x y z
		in
			(Interface(N c,x,y,z), ts'')
		end

fun parse ts =
	let
		val (p, ts') = parse_interface(tokenize (String.tokens delimitator ts))
	in 
		case ts' of
			nil => p 
			| _ => p
	end

val Interfaccia = parse I;
