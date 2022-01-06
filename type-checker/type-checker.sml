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
		| _ => raise TypeMismatch "sus"

fun find_dec(l, tipo) = 
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
							val args_types = check_args(el,params_types)
						in
							if params_types = args_types
							then ret
							else raise TypeMismatch "Paramaters passed to lambda do not agree"
						end
					| _ => raise TypeMismatch "Variable is not a lambda"
			end
		|typecheck_exp(G, Lambda(var_list, body), dl, t) = 
			let 
				val n = List.length var_list
				val tipo = getvalinterface(t)
				fun add_to_context(lt, vl) =
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
				val G = add_to_context(params_types, params) @ G
				val body_type = typecheck_exp(G, body, dl, t)
			in
				if body_type = ret andalso n = List.length params
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
		if t = e1_type then e2_type
		else raise TypeMismatch "Assignment types do not agree"
	end
