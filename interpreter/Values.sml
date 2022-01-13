datatype Values = Integer of int
				| Boolean of bool
				| Closure of char list * Expression * (char * Values) list