datatype Variable = Var of char;
datatype Name =  N of char;
datatype Type = ClassInterfaceType of Name | Int | Boolean;
datatype Expression = Cons of int | BoolCons of bool | VarExp of Variable | Plus of Expression * Expression | Lambda of Variable list * Expression | Apply of Variable * Expression list
datatype Declaration = Interface of Name * Type * Type list * Variable list;
datatype Program = Prog of Declaration list * Type * Variable * Expression * Expression;

datatype token = 
	TokenInterface of char | TokenIntType | TokenBoolType | TokenVar of char | TokenInterfaceType of char | TokenIntMethod | TokenBoolMethod
	| TokenCons of string | TokenBoolCons of string | TokenPlus | TokenLambda | TokenApply | TokenAssign | TokenEnd

exception InvalidToken of string
fun foo s =
	let 
		val c = (List.hd(String.explode s))
	in
		if Char.isUpper c then TokenInterfaceType c
		else if Char.isLower c then TokenVar c
		else if Char.isDigit c then TokenCons s
		else raise InvalidToken s
	end;

fun tokenize nil = nil 
	| tokenize ("interface" :: name :: r) = TokenInterface(List.hd(String.explode name)) :: tokenize r 
	| tokenize ("int" :: "m" :: r) = TokenIntMethod :: tokenize r
	| tokenize ("boolean" :: "m" :: r) = TokenBoolMethod :: tokenize r
	| tokenize ("int" :: v :: r) = TokenIntType :: TokenVar(List.hd(String.explode v)):: tokenize r
	| tokenize ("boolean" :: v :: r) = TokenBoolType :: TokenVar(List.hd(String.explode v)) :: tokenize r
	| tokenize ("false" :: r) = TokenBoolCons "false" :: tokenize r
	| tokenize ("true" :: r) = TokenBoolCons "true" :: tokenize r
	| tokenize ("->" :: r) = TokenLambda :: tokenize r
	| tokenize (".m" :: r) = TokenApply :: tokenize r
	| tokenize ("+" :: r) = TokenPlus :: tokenize r
	| tokenize ("=" :: r) = TokenAssign :: tokenize r
	| tokenize ("class" :: p :: "public" :: "static" :: "void" :: "main" :: "String[]" :: "args" :: r) = tokenize r
	| tokenize ("System" :: ".out" :: ".println" :: r) = tokenize r
	| tokenize ("}" :: r) = TokenEnd :: tokenize r
	| tokenize (s :: r) = foo s :: tokenize r

fun delimitator c = c = #" " 
		orelse 	c = #"," 
		orelse 	c = #"{" 
		orelse 	c = #"(" 
		orelse  c = #")"
		orelse 	c = #";";

fun replace c = 
	if c = #"." then " ." 
	else if c = #"+" then " + "
	else if c = #"-" then " -"
	else if c = #"}" then " } "
	else str c;