datatype Types = Integer of Type
				|Bool of Type
				|InterfaceType of Name * Type * Type list * Variable list

exception TypeMismatch of string
exception UnboundVariable of char
fun find (var, l : (char * 'a) list) =
  	case l of 
  		 [] => raise UnboundVariable var
     	| t :: l' => 
     		if (#1 t) = var
     		then (#2 t)
            else find(var, l')

fun getval t =
	case t of
		 Integer Int => Int
		|Bool Boolean => Boolean
		| _ => raise TypeMismatch "sos"

fun build_types t =
	case t of
		 Int => Integer Int
		|Boolean => Bool Boolean
		| _ => raise TypeMismatch "sos"

fun typecheck_exp(G, dl, Cons k) = Integer Int
		|typecheck_exp(G, dl, BoolCons b) = Bool Boolean
		|typecheck_exp(G, dl, VarExp(Var v)) = find(v, G)
		|typecheck_exp(G, dl, Plus(e1,e2)) = Integer Int
		|typecheck_exp(G, dl, Lambda(vl, body)) =
			let
				fun find_dec(l, n, t) = 
					case l of
						 [] => raise TypeMismatch "Functional interface does not exists"
						|Interface(name, ret, tlist, vlist) :: l' =>
							if n = List.length vlist andalso ret = t
							then
								InterfaceType(name, ret, tlist, vlist)
							else find_dec(l', n, t)
				fun add_to_contest(lt, vl) =
					case vl of
						 [] => []
						| Var(v) :: vl' => 
							let
								val t = List.hd lt
								val lt' = List.drop(lt,1)
							in
								(v, build_types(t)) :: add_to_contest(lt', vl')
							end
				val G = add_to_contest(tlist, vlist) @ G
				val body_type = getval(typecheck_exp(G, dl, body))
				val lambda_type = find_dec(dl, List.length vl, body_type)
			in
				lambda_type
			end
		|typecheck_exp(G, dl, Apply(Var(v), el)) = 
			let
				val var_t = find(v, G)
			in
				case var_t of 
					InterfaceType(name, ret_t, params_t, vlist) => build_types(ret_t)
			end

fun type_check(Prog(dl, t, Var(v), e1, e2)) = 
	let
		val G = []
		val e1_type = typecheck_exp(G, dl, e1)
		val G = (v, e1_type) :: G
		(*val e2_type = typecheck_exp(G, dl, e2)*)
	in 
		e1_type
	end