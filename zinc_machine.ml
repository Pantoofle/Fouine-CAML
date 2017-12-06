open Types
open Zinc_compiler
open List
open Machine_utils
open De_bruijn
open Machine_types
       
let debug = ref 0

(* Fonction principale d'evaluation *)
let rec eval code env astk rstk estk = 
  if (!debug = 2) then
    begin
      print_string "\n| Remaining code : "; print_lexem_list code stdout; print_newline();
      print_string "# Environnement : \n";
      let _ = print_env env in 
      print_string "# Stack : \n";
      let _ = print_stack astk in ();			       
    end;
  match code, astk, rstk, env, estk with
  | ACCESS(n)::c, _, _, _, _                                   -> eval c  env  (S_VAL(find_de_bruijn env n)::astk) rstk estk 
  | DUO::c, S_VAL(x)::S_VAL(y)::a, _, _, _                     -> eval c  env  (S_VAL(VAL_DUO (x, y))::a)       rstk estk 
  | CLOS(c')::c,  _, _, _, _                                   -> eval c  env  (S_VAL(VAL_FUN(c', env))::astk)     rstk estk
  | CLOS_R(c')::c,  _, _, _, _                                 -> eval c  env  (S_VAL(VAL_REC(c', env))::astk)     rstk estk
  | TAILAPPLY::c, S_VAL(VAL_FUN(c', e'))::a, _, _, _           -> eval c' e'   a                                   rstk estk
  | TAILAPPLY::c, S_VAL(VAL_REC(c', e'))::a, _, _, _           -> eval c' (VAL_REC(c', e')::e')   a                rstk estk 
  | APPLY::c, S_VAL(VAL_FUN(c', e'))::a, _, _, _               -> eval c' e'   a                                   (S_CODE(c)::S_ENV(env)::rstk) estk
  | APPLY::c, S_VAL(VAL_REC(c', e'))::a, _, _, _               -> eval c' (VAL_REC(c', e')::e')   a                (S_CODE(c)::S_ENV(env)::rstk) estk 
  | PUSHMARK::c, _, _, _, _                                    -> eval c  env  (S_MARK::astk)                      rstk estk 
  | GRAB::c, S_MARK::a, S_CODE(c')::S_ENV(e')::r, _, _         -> eval c' e'   (S_VAL(VAL_FUN(GRAB::c, env))::a)   rstk estk 
  | GRAB::c, S_VAL(v)::a, _, _, _                              -> eval c  (add_values env v) a                     rstk estk 
  | RETURN::c, S_VAL(VAL_FUN(c', e'))::a, _, _, _              -> eval c' e'   a                                   rstk estk 
  | RETURN::c, sval::S_MARK::a, S_CODE(c')::S_ENV(e')::r, _, _ -> eval c' e'   (sval::a)                           r    estk
  | RETURN::c, _, _, _, _                                      -> eval c  env  astk                                rstk estk 
  | SEQ::c, v::a, _, _, _                                      -> eval c  env  a                                   rstk estk 
  | CONST_INT(n)::c, _, _, _, _                                -> eval c  env  (S_VAL(VAL_INT n)::astk)            rstk estk 
  | ENDLET::c, _, _, _::e', _                                  -> eval c  e'   astk                                rstk estk
  | IF::c, S_VAL(VAL_BOOL b)::a, _, _, _                       -> if b then eval c env a rstk estk else eval (pop_code_until c ELSE) env a rstk estk 
  | ELSE::c, _, _, _, _                                        -> eval (pop_code_until c ENDIF) env astk           rstk estk 
  | ENDIF::c, _, _, _, _                                       -> eval c  env  astk                                rstk estk 
  | OP(op)::c, S_VAL(y)::S_VAL(x)::a, _, _, _                  -> eval c  env  (S_VAL(apply_operator op x y)::a)   rstk estk 
  | DEREF::c, S_VAL(VAL_REF(r))::a, _, _, _                    -> eval c  env  (S_VAL(!r)::a)                      rstk estk 
  | REF::c, S_VAL(v)::a, _, _, _                               -> let r = ref v in eval c  env  (S_VAL(VAL_REF r)::a) rstk estk 
  | ASSIGN::c, S_VAL(VAL_REF r)::S_VAL(y)::a, _, _, _          -> r := y;
								  eval c env   (S_VAL(y)::a)                         rstk estk
  | NOT::c, S_VAL(VAL_BOOL b)::a, _, _, _                      -> eval c  env  (S_VAL(VAL_BOOL (not b))::a)        rstk estk 
  | RAISE::c, _, _, _, (ce, cn, e', a', r')::estk'             -> eval cn e' ((eval ce env astk rstk estk')::a') r' estk'
  | TRY::c, S_VAL(VAL_FUN(c2, e))::a, _, _, _                  -> eval c  env  a                                   rstk ((c2, pop_code_until c ENDTRY, e, a, rstk)::estk)
  | ENDTRY::c, _, _, _, _::estk'                               -> eval c  env  astk                                rstk estk' 
  | FEEDBACK::c, S_VAL(v)::a, _, _, _                          -> print_value v; print_newline();
								  eval c  env  astk                                rstk estk
  | PRINT::c, S_VAL(VAL_INT n)::a, _, _, _                     -> print_int n; print_newline ();
								  eval c  env  astk                                rstk estk
  | [], _, _, _, _ -> hd astk
  | x::c, _, _, _, _-> failwith ("Wrong format when reading lexem "^(string_of_lexem x))

(* Frontend pour les appels depuis le main *)
let compute d c =
  debug := d;
  match eval c [] [] [] [] with
  | S_VAL(v) -> print_value v; print_newline ();
  | _ -> print_string "Not printable. Env, Code or empty stack"; print_newline ()
									       
let print_feedback debug ex =
  if (debug != 0) then print_string "Compilation :\n";
  let lexems = compiler ex in
  if (debug != 0) then print_string "Lancement de la machine à pile\n";
  if (debug != 0) then begin print_string "Lexems : ";
			     print_lexem_list lexems stdout; print_newline() end;
  compute debug lexems
	  
let compile_to_file debug file ex =
  if (debug != 0) then print_string "Compilation...";
  let lexems = compiler ex in
  let f = open_out_gen [Open_append; Open_creat] 744 (file) in
  if (debug != 0) then begin print_string "Lexems : ";
			     print_lexem_list lexems stdout; print_newline() end;
  print_lexem_list lexems f; Printf.fprintf f "\n"; flush f; close_out f;
  
