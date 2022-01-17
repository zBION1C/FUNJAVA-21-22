exception TypeMismatch of string
exception UnboundVariable of char
fun find (var, l : (char * 'a) list) =
  	case l of 
  		 [] => raise UnboundVariable var
     	| t :: l' => 
     		if (#1 t) = var
     		then (#2 t)
            else find(var, l')

fun getvalinterface t =
	case t of
		ClassInterfaceType(N(n)) => n
		| Int => #"t"
		| _ => raise TypeMismatch "Type declared do not match"

fun find_dec(l, tipo) = 
	(*
	cerca la dichiarazione di interfaccia corrispondente nella lista di dichiarazione di interfacce del programma
	usando come chiave il nome di interfaccia
	*)
	case l of
		 [] => raise TypeMismatch "Functional interface does not exists"
		|Interface(N(name), ret, tlist, vlist) :: l' =>
			if tipo = name
			then
				(name ,ret, tlist, vlist)
			else find_dec(l', tipo)

fun typecheck_exp(G, Cons k, dl, t) = Int
		|typecheck_exp(G, BoolCons b, dl, t) = Boolean
		|typecheck_exp(G, VarExp(Var v), dl, t) = find(v, G)
		|typecheck_exp(G, Plus(e1,e2), dl, t) = Int
		|typecheck_exp(G, Apply(Var(v), el), dl, t) = 
			let
				fun check_args(l,lt) =
					(*
					input: una lista di espressioni, e una lista di tipi
					output: una lista dei tipi delle espressioni in l. ogni espressioni l[i] e' tipata nel contesto G e passando il tipo lt[i]
					Funzione utile per andare a tipare le lambda nelle apply, di cui bisogna conoscere l'interfaccia corrispondente
					*)
					case l of
						[] => []
						| e :: l' => 
							let
								val t = List.hd lt
								val lt' = List.drop(lt, 1)
							in
								typecheck_exp(G, e, dl, t) :: check_args(l', lt')
							end
				val var_type = find(v, G)
			in 
				case var_type of
					ClassInterfaceType(N(name)) =>
						let 
							val (name, ret, params_types, params) = find_dec(dl, name)
							val args_types = check_args(el,params_types)				(*args_types corrisponde all'array dei tipi degli argomenti passati all'apply*)
						in
							if params_types = args_types 	(*Se i tipi passati corrispondono a quelli dichiarati*)
							then ret   						(*il tipo dell'apply e il tipo di ritorno dichiarato nell'interfaccia*)
							else raise TypeMismatch "Paramaters passed to lambda do not agree"
						end
					| _ => raise TypeMismatch "Variable is not a lambda"
			end
		|typecheck_exp(G, Lambda(var_list, body), dl, t) = 
			let 
				val n = List.length var_list
				val tipo = getvalinterface(t)
				fun add_to_context(lt, vl) =
					(*
					input: 2 liste lt e vl.
					output: array di coppie (lt[i], vl[i]) per ogni i.
					*)
					case vl of
						 [] => []
						| Var(v) :: vl' => 
							let
								val t = List.hd lt
								val lt' = List.drop(lt,1)
							in
								(v, t) :: add_to_context(lt', vl')
							end
				val (name, ret, params_types, params) = find_dec(dl, tipo)
				val G = add_to_context(params_types, var_list) @ G				(*Il corpo della lambda va tipato nel contesto G, aumentandolo associando a tutte le variabili di input della lambda, il tipo dichiarato nell'interfaccia corrsipondente*)
				val body_type = typecheck_exp(G, body, dl, t)	
			in
				if body_type = ret andalso n = List.length params  				(*se il tipo del corpo trovato corrisponde al tipo di ritorno dichiarato e il numero di argomenti passati corrisponde al numero di argomento dichiarati*)
				then 
					ClassInterfaceType(N(name))
				else raise TypeMismatch "Lambda does not match the interface declaration"
			end


fun type_check(Prog(dl, t, Var(v), e1, e2)) = 
	let
		val G = []
		val e1_type = typecheck_exp(G, e1, dl, t)
		val G = (v, e1_type) :: G
		val e2_type = typecheck_exp(G, e2, dl, t)
	in 
		if t = e1_type then e1_type
		else raise TypeMismatch "Assignment types do not agree"
	end