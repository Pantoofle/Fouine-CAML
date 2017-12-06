open Types
open Machine_utils
open Machine_types
       
(* Le compilateur à proprement parler *)
let rec to_lexem_list_aux ex memory with_ret =
  (* Les structures et quelques fonctions pour les manipuler *)
  let compiled:(code ref) = ref (if with_ret then [RETURN] else []) in
  let push l = compiled := l::(!compiled) in
  let rec find_v mem e =
    match e, mem with
    | Name(n), []
    | Reserved(n), []
    | Underscore(n), [] -> failwith (n^" not found in environement during the compilation")
    | _, x::q -> if(x = e) then 0 else 1 + (find_v q e)
  in
  
  (* La partie recursive du compiler *)
  let rec empile c m = 
    match c with
    | Int(x)               -> push (CONST_INT(x))
    | Op(op, c1, c2)       -> push (OP(op)); empile c1 m; empile c2 m
    | Let(Ass(nom, v), ex) -> push (ENDLET); empile ex (nom::m); push (GRAB); empile v m
    | Rec(Ass(nom_f, v), ex) ->
       begin
	 match v with
	 | Fun(nom_v, ex_f) -> push (ENDLET); empile ex (nom_f::m); push (GRAB);
			       push (CLOS_R(to_lexem_list_aux ex_f (nom_v::nom_f::m) true));
	 | _ -> failwith "Only functions can be recursive"
       end
    | Fun(nom, ex)         -> push (CLOS(to_lexem_list_aux ex (nom::m) true))
    | Var(nom)             -> push (ACCESS(find_v m nom))
    | Apply(Var(Name "prInt" ), c2) -> push(PRINT); empile c2 m
    | Apply(c1, c2)        -> push (APPLY); empile c1 m; empile c2 m
    | Seq(c1, c2)          -> empile c2 m; push (SEQ); empile c1 m
    | Ifte(cond, c1, c2)   -> push (ENDIF); empile c2 m; push (ELSE);
			      empile c1 m; push (IF); empile cond m 
    | Deref(c1)            -> push (DEREF); empile c1 m
    | Assign(nom, v)       -> push (ASSIGN(find_v m nom)); empile v m
    | Ref(c1)              -> push (REF); empile c1 m
    | Not(c1)              -> push (NOT); empile c1 m
    | Raise(c1)            -> push (RAISE); empile c1 m
    | Try(c1, Ass(nom, c2))-> push (ENDTRY); empile c1 m; push (TRY);
			      push (CLOS(to_lexem_list_aux c2 (nom::m) false))
    | Print(c1)            -> push (PRINT); empile c1 m
    | Feedback(c1)         -> push (FEEDBACK); empile c1 m
  in
  empile ex memory;
  !compiled

let compile ex =
  to_lexem_list_aux ex [] false


let string_of_char =
  String.make 1

	      
(* Fonctions d'affichage qui servent aussi à l'export lors du passage de -interm *)
let rec print_lexem l file =
  match l with
  | ACCESS (n) -> Printf.fprintf file "ACCESS(%d)" n
  | GRAB       -> Printf.fprintf file "GRAB"
  | CLOS (c)   -> Printf.fprintf file "CLOS(";			  
		  print_lexem_list c file; Printf.fprintf file ")"
  | CLOS_R (c)-> Printf.fprintf file "CLOS_R(";			  
		 print_lexem_list c file; Printf.fprintf file ")"
  | CONST_INT(x)       -> Printf.fprintf file "CONST_INT(%d)" x
  | OP(op)             -> Printf.fprintf file "OP(%s)"
					 begin
					   match op with
					   | '&' -> "&&"
					   | '|' -> "||"
					   | 'l' -> "<="
					   | 'g' -> ">="
					   | '#' -> "<>"
					   | _   -> string_of_char op
					 end
  | APPLY              -> Printf.fprintf file "APPLY"
  | ENDLET             -> Printf.fprintf file "ENDLET"
  | IF                 -> Printf.fprintf file "IF"	
  | ELSE               -> Printf.fprintf file "ELSE"
  | ENDIF              -> Printf.fprintf file "ENDIF"					 
  | RETURN             -> Printf.fprintf file "RETURN"
  | PRINT              -> Printf.fprintf file "PRINT"
  | NOT                -> Printf.fprintf file "NOT"
  | TRY                -> Printf.fprintf file "TRY"
  | ENDTRY             -> Printf.fprintf file "ENDTRY"
  | RAISE              -> Printf.fprintf file "RAISE"
  | _                  -> Printf.fprintf file "_"
and print_lexem_list l file =
  match l with
  | a::q -> print_lexem a file; Printf.fprintf file ";";
	    print_lexem_list q file;
  | [] -> ()

