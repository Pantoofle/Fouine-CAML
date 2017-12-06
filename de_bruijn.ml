open Types

let rec push_pattern_names e p = 
  match p with
  | Pat_name(Name n) -> n::e
  | Pat_fun(f::_) -> push_pattern_names e f
  | Pat_mark(p, _) -> push_pattern_names e p
  | Pat_duo(p, q) -> push_pattern_names (push_pattern_names e p) q
  | _ -> e

		  
let rec push_pattern_list e l =
  match l with
  | [] -> e
  | x::q -> push_pattern_list (push_pattern_names e x) q 

let rec push_pattern_args e p =
  match p with
  | Pat_fun(_::q) -> push_pattern_list e q
  | _ -> e

	   
(* Traduit une expression classique en une expression en indices de de Bruijn. 
 * Les termes en Ass(nom, valeur) sont conservés mais le champ "nom" ne sera pas traité
 * puisque l'acces aux variables se fera à partir de l'indice qui a été donné*)
let rec tree_to_bruijn code env =
  let rec find_v mem nom =
    match mem with
    | [] -> failwith (nom^" not found in the environnement")
    | x::q -> if(x = nom) then 0 else 1 + (find_v q nom)
  in
  match code with
  | Int(x)               -> Int(x)
  | Duo(x, y)            -> Duo(tree_to_bruijn x env, tree_to_bruijn y env)
  | Ref(x)               -> Ref(tree_to_bruijn x env)
  | Op(op, x, y)         -> Op(op, tree_to_bruijn x env, tree_to_bruijn y env)
  | Not(x)               -> Not(tree_to_bruijn x env)
  | Let(Ass(p, x), y)    -> Let(Ass(p, tree_to_bruijn x (push_pattern_args env p)),
				tree_to_bruijn y (push_pattern_names env p))
  | Rec(Ass(p, x), y)    -> Rec(Ass(p, tree_to_bruijn x (push_pattern_args (push_pattern_names env p) p)),
				tree_to_bruijn y (push_pattern_names env p))
  | Fun(p, x)            -> Fun(p, tree_to_bruijn x (push_pattern_names env p))
  | Var(Name n)          -> Var(Name(string_of_int (find_v env n)))
  | Var(nom)             -> Var(nom)
  | Deref(x)             -> Deref(tree_to_bruijn x env)
  | Assign(x, y)         -> Assign(tree_to_bruijn x env, tree_to_bruijn y env)
  | Apply(x, y)          -> Apply(tree_to_bruijn x env, tree_to_bruijn y env)
  | Seq(x, y)            -> Seq(tree_to_bruijn x env, tree_to_bruijn y env)
  | Ifte(cond, x, y)     -> Ifte(tree_to_bruijn cond env,
				 tree_to_bruijn x env, tree_to_bruijn y env)
  | Raise(x)             -> Raise(tree_to_bruijn x env)
  | Try(x, Ass(p, y))    -> Try(tree_to_bruijn x env,
				Ass(p, tree_to_bruijn y (push_pattern_names env p)))
  | Print(x)              -> Print(tree_to_bruijn x env)
  | Feedback(x)           -> Feedback(tree_to_bruijn x env)  

(* Acceder à la valeur d'un element repéré par son indice *)
let rec find_de_bruijn e n =
  match e with
  | [] -> failwith "Env: Out of bounds !"
  | x::q -> if (n = 0) then x else find_de_bruijn q (n-1) 
