datatype Variable = Var of char;
datatype Name =  N of char;
datatype Type = ClassInterfaceType of Name | Int | Boolean;
datatype Expression = Cons of int | BoolCons of int | VarExp of Variable | Plus of Expression * Expression | Lambda of Variable list * Expression | Apply of Variable * Expression list
datatype Declaration = Interface of Name * Type * Type list * Variable list;
datatype Program = Prog of Declaration list * Type * Variable * Expression * Expression;