open Types
open List
open Machine_types					
       
let pop e =
  match !e with
  | [] -> failwith "Empty stack or env, cannot pop"
  | a::q -> e := q; a

let push e x =
  e := x::(!e)
	    
let reverse c =
  let rec r c1 c2 = 
    match c1 with
    | [] -> c2
    | x::q -> r q (x::c2)
  in r c []


(* Pour dépiler les IF et TRY quand on a besoin de sauter une partie du code *)
let rec pop_code_until c l =
  match c with
  | [] -> failwith "Lexem not found !"
  | a::q ->
     begin
       match a with
       | IF  -> pop_code_until (pop_code_until q ENDIF) l
       | TRY ->pop_code_until (pop_code_until q ENDTRY) l
       | _ -> if (a = l) then q else pop_code_until q l
     end
       
(* Fonctions d'affichage, pour le debug et l'affichage de la valeur de retour *)
       
let string_of_char =
  String.make 1


let string_of_lexem l =
  match l with
  | ACCESS(_)    -> "ACCESS"
  | GRAB         -> "GRAB"
  | ASSIGN       -> "ASSIGN"
  | CLOS(_)      -> "CLOS"
  | CLOS_R (_)   -> "CLOS_R"
  | CONST_INT(_) -> "CONST_INT"
  | OP(_)        -> "OP"
  | DUO          -> "DUO"
  | SEQ          -> "SEQ"
  | TRY          -> "TRY"
  | ENDTRY       -> "ENDTRY"
  | RAISE        -> "RAISE"
  | REF          -> "REF"
  | DEREF        -> "DEREF"
  | APPLY        -> "APPLY"
  | TAILAPPLY    -> "TAILAPPLY"
  | PUSHMARK     -> "PUSHMARK"
  | NOT          -> "NOT"
  | PRINT        -> "PRINT"
  | FEEDBACK     -> "FEEDBACK"
  | ENDLET       -> "ENDLET"
  | IF           -> "IF"
  | ELSE         -> "ELSE"
  | ENDIF        -> "ENDIF"
  | RETURN       -> "RETURN"
    
(* Fonctions d'affichage qui servent aussi à l'export lors du passage de -interm *)
let rec print_lexem l file =
  match l with
  | ACCESS (n) -> Printf.fprintf file "ACCESS(%d)" n
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
  | DUO                -> Printf.fprintf file "DUO"
  | GRAB               -> Printf.fprintf file "GRAB"
  | ASSIGN             -> Printf.fprintf file "ASSIGN"
  | REF                -> Printf.fprintf file "REF"
  | DEREF              -> Printf.fprintf file "DEREF"
  | FEEDBACK           -> Printf.fprintf file "FEEDBACK"
  | SEQ                -> Printf.fprintf file "SEQ"
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
  | TAILAPPLY          -> Printf.fprintf file "TAILAPPLY"
  | PUSHMARK           -> Printf.fprintf file "PUSMARK"
and print_lexem_list l file =
  match l with
  | a::q -> print_lexem a file; Printf.fprintf file ";";
	    print_lexem_list q file;
  | [] -> ()
	    
let rec print_value v =
  match v with
  | VAL_INT(x) -> print_string "<integer> : "; print_int(x)
  | VAL_REF(x) -> print_string "<ref>"; print_value(!x)
  | VAL_BOOL(x) -> print_string "<boolean> : ";
		   if x then print_string("true") else print_string("false") 
  | VAL_FUN(c, _) ->
     print_string "<function> ";
     print_string " : ";
     print_lexem_list c stdout;
  | VAL_DUO(x, y) ->
     print_string "<duo> : (";
     print_value x;
     print_char ',';
     print_value y;
     print_char ')';
  | VAL_REC(c, _) ->
     print_string "<rec><function> ";
     print_string " : ";
     print_lexem_list c stdout

(* Affiche tous les bindings dans l'environement. Pas utilisé dans la version finale mais utile pour debug *)
let print_env e =
  let rec r env n = 
    match env with
    | [] -> ()
    | v::q -> print_int n; print_string " -> ";
	      print_value v; print_newline(); r q (n+1)
  in
  r e 0
    
let print_stack s =
  let rec r s n = 
    match s with
    | [] -> ()
    | S_VAL(v)::q -> print_int n; print_string " -> ";
		     print_value v; print_newline(); r q (n+1)
    | S_MARK::q -> print_int n; print_string " -> ";
		   print_string "MARK\n"; r q (n+1)
    | S_CODE(_)::q -> print_int n; print_string " -> ";
		      print_string "CODE\n"; r q (n+1)
    | S_ENV(_)::q -> print_int n; print_string " -> ";
		     print_string "ENV\n"; r q (n+1)
  in
  r s 0
    

(* Petite sous fonction pour le match de l'operateur booléen et unaire. Idem, c'est juste pour alleger eval *)
let apply_operator op x y =
  match x, y with
  | VAL_INT(a), VAL_INT(b)  ->
     begin
       match op with
       | '+' -> VAL_INT(a + b)
       | '-' -> VAL_INT(a - b)
       | '*' -> VAL_INT(a * b)
       | '/' -> VAL_INT(a / b)
       | '=' -> VAL_BOOL(a = b)
       | '<' -> VAL_BOOL(a < b)
       | '>' -> VAL_BOOL(a > b)
       | 'l' -> VAL_BOOL(a <= b)
       | 'g' -> VAL_BOOL(a >= b)
       | '#' -> VAL_BOOL(a != b)
       | _ -> failwith "wrong Integer operator"
     end
  | VAL_BOOL(a), VAL_BOOL(b) ->
     begin
       match op with
       | '&' -> VAL_BOOL(a && b)
       | '|' -> VAL_BOOL(a || b)
       | _ -> failwith "wrong Boolean operator"
     end
  |_ -> failwith "wrong operandes"

let rec add_values env v =
  match v with
  | VAL_DUO(x, y) -> add_values (add_values env y) x
  | _ -> v::env
