open Types
open Compiler
open List
open Machine_utils
open Machine_types
open De_bruijn

let return s =
  let sv = pop s in
  let sc = pop s in
  let se = pop s in
  match sv, sc, se with
  | S_VAL(_), S_CODE(c'), S_ENV(e') ->
     push s sv;
     (* print_string "Returning\n"; print_env (e'); *)
     (c', e')
  | _ -> failwith "Return failed, no env or code to load"

(* Fonction principale d'evaluation *)
let eval c e debug =
  let s:stack = ref [] in
  let errors:error_caller = ref [] in
  let rec ev c e =
    match c with
    | [] -> pop s
    | l::q ->
       if (debug = 2) then
	 begin
	   print_string "\n| Remaining code : "; print_lexem_list c stdout; print_newline();
	   print_string "# Environnement : \n";
	   let _ = print_env e in 
	   print_string "# Stack : \n";
	   let _ = print_stack (!s) in ();			       
	 end;
       match l with

       | SEQ -> let _ = pop s in ev q e
       | RETURN ->
	  let c', e' = return s in
	  ev c' e'
       | ACCESS(n)        -> push s (S_VAL(find_de_bruijn e n)); ev q e
       | GRAB             ->
	  begin
	    match pop s with
	    | S_VAL(a) -> ev q (a::e);
	    | _ -> failwith "Wrong LET format"
	  end

       | CLOS(c1)         -> push s (S_VAL(VAL_FUN(c1, e))); ev q e
       | CLOS_R(c1)       -> push s (S_VAL(VAL_REC(c1, e))); ev q e
       | CONST_INT(x)     -> push s (S_VAL(VAL_INT(x))); ev q e

       | APPLY            ->
	  begin
	    match pop s with
	    | S_VAL(VAL_FUN(c1, e'))->
	       begin
		 match pop s with
		 | S_VAL(v) ->
		    push s (S_ENV(e));
		    push s (S_CODE(q));
		    ev c1 (v::e')
		 |_-> failwith "Wrong APPLY format while getting arguments"
	       end
	    | S_VAL(VAL_REC(c1, e')) ->
	       begin
		 match pop s with
		 | S_VAL(v) ->
		    push s (S_ENV(e));
		    push s (S_CODE(q));
		    ev c1 (v::VAL_REC(c1, e')::e')
		 |_-> failwith "Wrong APPLY format while getting arguments"
	       end
	    | _ -> failwith "Wrong APPLY format while getting the function"
	  end
       | ENDLET           -> ev q (tl e)

       | IF ->
	  begin
	    match pop s with
	    | S_VAL(VAL_BOOL(true)) -> ev q e;
	    | S_VAL(VAL_BOOL(false)) -> ev (pop_code_until q (ELSE)) e
	    | _ -> failwith "Wrong IFTE format"
	  end
       | ELSE        -> ev (pop_code_until q (ENDIF)) e
       | ENDIF       -> ev q e;

       | OP(c)       ->
	  begin
	    match !s with
	    | S_VAL(x)::S_VAL(y)::p -> s := S_VAL(apply_operator c x y)::p;
				       ev q e
	    | _ -> failwith "Wrong Operande"
	  end
       | DEREF       ->
	  begin
	    match pop s with
	    | S_VAL(VAL_REF(v)) -> push s (S_VAL(!v)); ev q e
	    | _ -> failwith "Not a reference !" 
	       end
       | REF         ->
	  begin
	    match pop s with
	    | S_VAL(a) -> let v = ref a in push s (S_VAL(VAL_REF(v))); ev q e;
	    | _ -> failwith "Wrong REF format"
	  end
       | ASSIGN(n) ->
	  begin
	    match find_de_bruijn e n, hd (!s) with
	    | VAL_REF(v), S_VAL(v2) -> v := v2; ev q e
	    | _ -> failwith "Wrong assignment" 
	  end
       | NOT ->
	  begin
	    match pop s with
	    | S_VAL(VAL_BOOL(b)) -> push s ( S_VAL(VAL_BOOL(not b)) ); ev q e
	    | _ -> failwith "Can only apply Not to a boolean"
	  end

       | RAISE ->
	  begin
	    match pop s with
	    | S_VAL(v) -> 
	       let c_err, c_next, e2 =
		 try pop errors
		 with _ ->
		   begin
		     match v with
		     | VAL_INT(x) ->
			failwith ("Raised error "^(string_of_int x)^" and not caught !")
		     | _-> failwith "Raised error and not caught !"
		   end
	       in begin
		 push s (ev c_err (v::e2)); 
		 ev c_next e2
	       end
	    | _ -> failwith "Raise an int value, not code or env"
	  end
       | TRY ->
	  begin
	    match pop s with
	    | S_VAL(VAL_FUN(c, e')) ->
	       push errors (c, (pop_code_until q ENDTRY), e')
	    | _ -> failwith "Can only try with a function"
	  end;
	  ev q e
       | ENDTRY -> let _ = pop errors in ev q e

       | FEEDBACK ->
	  begin
	    match hd (!s) with
	    | S_VAL(v) -> print_value v; print_newline(); ev q e
	    | _ -> failwith "Can't feedback a code or env"
	  end
       | PRINT ->
	  begin
	    match hd (!s) with
	    | S_VAL(VAL_INT(v)) -> print_int v; print_newline(); ev q e
	    | _ -> failwith "Can't prInt a non-int !"
	  end
       | TAILAPPLY | PUSHMARK -> failwith "Des lexems ZINC dans la classique ? Comment ?"
  in
  ev c e
     
(* Frontend pour les appels depuis le main *)
let compute debug c =
  let e = [] in
  match eval c e debug with
  | S_VAL(v) -> print_value v; print_newline ();
  | _ -> print_string "Not printable. Env or Code"; print_newline ()
								  
let print_feedback debug ex =
  if (debug != 0) then print_string "Compilation...";
  let lexems = compile ex in
  if (debug != 0) then print_string " Done !\nLancement de la machine à pile\n";
  if (debug != 0) then begin print_string "Lexems : ";
			     print_lexem_list lexems stdout; print_newline() end;
  compute debug lexems
	  
let compile_to_file debug file ex =
  if (debug != 0) then print_string "Compilation...";
  let lexems = compile ex in
  let f = open_out_gen [Open_append; Open_creat] 744 (file) in
  if (debug != 0) then begin print_string "Lexems : ";
			     print_lexem_list lexems stdout; print_newline() end;
	     print_lexem_list lexems f; Printf.fprintf f "\n"; flush f; close_out f;
