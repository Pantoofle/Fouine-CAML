open Types;;
open Interprete_aux;;

(* Nota Bene : Il est conseillé de lire interprete_aux.ml
 * avant de lire ce fichier
 *)

(* Affiche le contenu d'un binding.
 * C'est cette fonction qui constitue en grande partie ce qui
 * est affiché à la fin de l'interprétation d'une expression.
 *)
let print_of_binding = function
  |Val_binding(pat,v,m) ->
     print_pattern pat; print_string " : "; print_mark m;
    print_string " = "; print_value v; print_newline ()
  |Raw_binding _ -> failwith "No es possible, felipe"
;;


(* Construit un Raw_binding à partir d'un assignement 'ass' et
 * d'un environnement 'env'
 *)
let raw_binding_of_ass_env: ass -> env -> binding =
  function Ass(pat,expr) -> fun env ->
    let (pat,mark) = ae_enlever_mark pat in
    let (pat,expr) = ae_enlever_fun (pat,expr) in
    Raw_binding(pat,expr,env,mark)
;;


(* Modifie l'environnelent 'env' pour que la variable de nom 'name'
 * et de type ref contienne désormais la valeur 'value'
 *)
let rec env_assign: name -> value -> env -> unit =
  fun name value -> function
  |[] -> failwith ("Erreur d'assignement, pas de " ^ (string_of_name name)  ^
     " de type référence dans l'environnement actuel.")
  |x::s -> match x with
    |Val_binding(p,Val_ref v,_) when p=Pat_name name -> v:=value
    |_ -> env_assign name value s
;;

(* Déréférence un Ref_binding *)
let deref = function
  |Val_binding(p,Val_ref v,REF m) -> Val_binding(p,!v,m)
  |Val_binding _ -> failwith "On ne peut pas déréférencer une valeur"
  |Raw_binding _ -> failwith "This can't happened"
;;

(* 'binding_of_expr_env'  prend  une  expression  'expr'  et  un *
 * environnement 'env',  et renvoie  un binding  correspondant à *
 * l'interprétation de cette expression.                         *
 *                                                               *
 * 'val_binding_of_ass_env' renvoie un Val_binding à partir d'un *
 * assignement 'ass' et d'un environnement 'env'.                *
 *                                                               * 
 * 'val_of_raw' convertit un Raw_binding en Val_binding.         *
 * La précense de cette fonction vient du fait qu'une évaluation *
 * paresseuse est effectué lors d'un  "let rec".  Cette fonction *
 * est  donc appelée  dès qu'il devient nécessaire  de connaître *
 * la valeur de ce Raw_binding.                                  *
 *)

let rec binding_of_expr_env: expr -> env -> binding = fun expr env ->
  match expr with
    
  |Int n     -> Val_binding(Anonymous,Val_int n,INT)
  |Duo(a,b)  ->
     let bind_b = val_of_raw (binding_of_expr_env b env)
     and bind_a = val_of_raw (binding_of_expr_env a env) in
     begin match (bind_a,bind_b) with
     |Val_binding(_,va,ma), Val_binding(_,vb,mb) ->
	Val_binding(Anonymous, Val_duo(va,vb), DUO(ma,mb))
     |Val_binding(_,Val_err _, _) as err, _   -> err
     |_, (Val_binding(_,Val_err _, _) as err) -> err
     |_ -> raise Type_error
     end

  |Op(c,a,b) ->
     let bind_b = val_of_raw (binding_of_expr_env b env)
     and bind_a = val_of_raw (binding_of_expr_env a env) in
     begin match (bind_a,bind_b) with
     |Val_binding(_,va,ma), Val_binding(_,vb,mb) ->
	let (v,m) = do_op c va vb in
	Val_binding(Anonymous, v, m)
     |Val_binding(_,Val_err _, _) as err, _   -> err
     |_, (Val_binding(_,Val_err _, _) as err) -> err
     |_ -> raise Type_error
     end
  |Not e ->
     let bind = val_of_raw (binding_of_expr_env e env) in
     begin match bind with
     |Val_binding(_,Val_bool b, m) ->
	Val_binding(Anonymous, Val_bool (not b) , m)
     |Val_binding(_,Val_err _, _) as err -> err
     |_ -> raise Type_error
     end
       
  |Let(a,f)   -> let b = val_binding_of_ass_env a env in
		 if est_erreur b then b
		 else binding_of_expr_env f (env_push env (bind_list_of_bind b))
  |Rec(a,f)   -> let b = raw_binding_of_ass_env a env in
		 if est_erreur b then b
		 else binding_of_expr_env f (env_push env [b])
  |Fun(p,e)   -> let (p,m) = fun_enlever_mark p in
		 Val_binding (Anonymous, Val_fun(p,e,env),
			      if m=PRIME then FUN(PRIME,PRIME) else m)
  |Var n      -> env_lookup env n
  |Deref e    -> let b = val_of_raw (binding_of_expr_env e env) in
		 if est_erreur b then b else deref b

  |Apply(f,x) ->
     let bind_x = val_of_raw (binding_of_expr_env x env) in
     let bind_f = binding_of_expr_env f env in
     begin match (bind_x, bind_f) with
     |v, Val_binding(_,Val_fun(y,fy,e),FUN(m,_)) ->
	let (y,fy) = fun_enlever_fun (y,fy) in
	binding_of_expr_env fy
	  (env_push e (bind_list_of_bind (unifier_mark m (set_pattern y v))) )
     |v, (Raw_binding(_,Fun(y,fy),e,FUN(m,_)) as r) ->
	binding_of_expr_env fy (env_push e [r;unifier_mark m (set_pattern y v)])
     |Val_binding(_,Val_err _,_) as err, _   -> err
     |_, (Val_binding(_,Val_err _,_) as err) -> err
     |v, (Raw_binding(_,Fun(y,fy),e,_) as r) ->
	binding_of_expr_env fy (env_push e [r;set_pattern y v])
     (* Ce cas ne devrait pas être là, mais il permet de faire que les 'let rec' marchent même si leur typage ne marche pas *)
     |_ -> failwith "Une valeur ne peut pas être appliquée à une autre valeur."
     end
  |Seq(e,f)    ->
     let b = val_of_raw (binding_of_expr_env e env) in
     if est_erreur b then b else binding_of_expr_env f env
  |Ifte(e,f,g) ->
     begin match val_of_raw (binding_of_expr_env e env) with
     |Val_binding(_,Val_bool b,_) ->
	if b
	then binding_of_expr_env f env
	else binding_of_expr_env g env
     |Val_binding(_,Val_err _,_) as err -> err
     |_ -> raise Type_error
     end
       
  |Assign(f,e) ->
     let b = val_of_raw (binding_of_expr_env e env) in
     let d = val_of_raw (binding_of_expr_env f env) in
     if est_erreur b then b
     else if est_erreur d then d
     else begin
       let _,value,_ = content_of_val_bind b in
       let pat,_,_   = content_of_val_bind d in
       match pat with
       |Pat_name n -> env_assign n value env; b
       |Anonymous  -> b
       |_ -> raise Type_error
     end
  |Ref e       ->
     let b = val_of_raw (binding_of_expr_env e env) in
     if est_erreur b then b else
       begin
	 let _,value,mark = content_of_val_bind b in
	 Val_binding(Anonymous,Val_ref(ref value),REF mark)
       end
  |Feedback e  ->
     let b = val_of_raw (binding_of_expr_env e env) in
     print_of_binding b; b
  |Print e     ->
     let b = val_of_raw (binding_of_expr_env e env) in
     begin
       match b with
       |Val_binding(_,Val_int n,_) -> print_int n ; print_newline (); b
       |Val_binding(_,Val_err _,_) as err -> err
       |_ -> failwith "Vous ne pouvez afficher que des entiers avec prInt"
     end
       
  |Raise e     ->
     let b = val_of_raw (binding_of_expr_env e env) in
     begin
       match b with
       |Val_binding(_,Val_int n,_) -> Val_binding(Anonymous,Val_err n,PRIME)
       |_ -> failwith "Seuls des entiers peuvent être soulevés"
     end
  |Try(e,Ass(Pat_name name,f)) ->
     let b = val_of_raw (binding_of_expr_env e env) in
     begin
       match b with
       |Val_binding(_,Val_err n,_) ->
	  binding_of_expr_env f
	    (env_push env [Val_binding(Pat_name name,Val_int n,INT)])
       |_ -> b
     end
  |Try _ -> failwith "Has to be a name"
     
and val_binding_of_ass_env: ass -> env -> binding =
  function Ass(pat,expr) -> fun env ->
    let (pat,mark) = ae_enlever_mark pat in
    let (pat,expr) = ae_enlever_fun (pat,expr) in
    let val_bind   = val_of_raw (binding_of_expr_env expr env) in
    match val_bind with
    |Val_binding(_,value,m) -> Val_binding(pat,value,unification mark m)
    |_ -> failwith "Quite impossible, as I just did a val_of_raw"
    
and val_of_raw = function
  |Val_binding _ as v -> v
  |Raw_binding(Pat_name name as p,f,e,m) ->
     val_of_raw (set_mark m (set_pattern p (binding_of_expr_env f e)))
  |_ -> failwith
     "I don't want Pat_x, where 'x' is not 'name', in my env; so get out the way"
;;

let binding_of_expr expr = binding_of_expr_env expr (empty_env ());;

(* C'est cette fonction qui est appelée à la fin
 * de l'interprétation d'une expression. 
 *)
let print_feedback expr = print_of_binding (val_of_raw (binding_of_expr expr));;
