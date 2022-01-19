datatype token = 
	TokenInterface of char | TokenIntType | TokenBoolType | TokenVar of char | TokenInterfaceType of char | TokenIntMethod | TokenBoolMethod
	| TokenCons of string | TokenBoolCons of string | TokenPlus | TokenLambda | TokenApply | TokenComma | TokenEnd | LPAREN | RPAREN | TokenInterfaceMethod of char

exception InvalidToken of string
(* Funzione che riconosce e tokenizza i nomi delle variabili, di interfaccia e i numeri interi *)
fun tokenize_alfanumeric s =
	let 
		val c = (List.hd(String.explode s))
	in
		if Char.isUpper c then TokenInterfaceType c
		else if Char.isLower c then TokenVar c
		else if Char.isDigit c then TokenCons s
		else raise InvalidToken s
	end;

(* Funzione che tokenizza la lista di stringhe che rappresenta il programma FUNJAVA *)
fun tokenize nil = nil 
	| tokenize ("interface" :: name :: r) = TokenInterface(List.hd(String.explode name)) :: tokenize r 
	| tokenize ("int" :: "m" :: r) = TokenIntMethod :: tokenize r
	| tokenize ("boolean" :: "m" :: r) = TokenBoolMethod :: tokenize r
	| tokenize (i :: "m" :: r) = TokenInterfaceMethod (List.hd(String.explode i)) :: tokenize r
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

(* Funzione usata nella String.tokens per spezzare la stringa in una lista escludendo i delimitatori *)
fun delimitator c = c = #" " 
		orelse 	c = #"{" 
		orelse  c = #"="
		orelse 	c = #"}"
		orelse  c = #"\t"
		orelse  c = #"\n";

(* Funzione usata nella String.replace per facilitare il riconoscimento di simboli chiave del programma *)
fun replace c = 
	if c = #"." then " ." 
	else if c = #"+" then " + "
	else if c = #"-" then " -"
	else if c = #";" then " ; "
	else if c = #"," then " , "
	else if c = #"(" then " ( "
	else if c = #")" then " ) "
	else str c;