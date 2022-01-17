datatype token = 
	TokenInterface of char | TokenIntType | TokenBoolType | TokenVar of char | TokenInterfaceType of char | TokenIntMethod | TokenBoolMethod
	| TokenCons of string | TokenBoolCons of string | TokenPlus | TokenLambda | TokenApply | TokenComma | TokenEnd | LPAREN | RPAREN | TokenInterfaceMethod

exception InvalidToken of string
fun tokenize_alfanumeric s =
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
	| tokenize (i :: "m" :: r) = TokenBoolMethod :: tokenize r
	| tokenize ("int" :: v :: r) = TokenIntType :: TokenVar(List.hd(String.explode v)):: tokenize r
	| tokenize ("boolean" :: v :: r) = TokenBoolType :: TokenVar(List.hd(String.explode v)) :: tokenize r
	| tokenize ("false" :: r) = TokenBoolCons "false" :: tokenize r
	| tokenize ("true" :: r) = TokenBoolCons "true" :: tokenize r
	| tokenize ("->" :: r) = TokenLambda :: tokenize r
	| tokenize (".m" :: r) = TokenApply :: tokenize r
	| tokenize ("+" :: r) = TokenPlus :: tokenize r
	| tokenize ("," :: r) = TokenComma :: tokenize r
	| tokenize ("class" :: p :: "public" :: "static" :: "void" :: "main" :: "(" :: "String[]" :: "args" :: ")" :: r) = tokenize r
	| tokenize ("System" :: ".out" :: ".println" :: "(" :: r) = tokenize r
	| tokenize (";" :: r) = TokenEnd :: tokenize r
	| tokenize ("(" :: r) = LPAREN :: tokenize r
	| tokenize (")" :: r) = RPAREN :: tokenize r
	| tokenize (s :: r) = tokenize_alfanumeric s :: tokenize r

fun delimitator c = c = #" " 
		orelse 	c = #"{" 
		orelse  c = #"="
		orelse 	c = #"}"
		orelse  c = #"\t"
		orelse  c = #"\n";

fun replace c = 
	if c = #"." then " ." 
	else if c = #"+" then " + "
	else if c = #"-" then " -"
	else if c = #";" then " ; "
	else if c = #"," then " , "
	else if c = #"(" then " ( "
	else if c = #")" then " ) "
	else str c;