open Types;;

(* mark.ml *)

exception Unification_impossible ;;

let rec mark_of_mark_list = fun mb -> function
  |[]   -> mb
  |m::s -> FUN(m,mark_of_mark_list mb s)
;;

let rec unification : mark -> mark -> mark = fun m n -> match (m,n) with
  |INT, INT       -> INT
  |BOOL, BOOL     -> BOOL
  |REF m', REF n' -> REF (unification m' n')
  |FUN(m1,m2), FUN(n1,n2) -> FUN(unification m1 n1,unification m2 n2)
  |DUO(m1,m2), DUO(n1,n2) -> DUO(unification m1 n1,unification m2 n2)
  |PRIME, _ -> n
  |_ ,PRIME -> m
  |_ -> raise Unification_impossible
;;

(* EOF *)


exception Type_error;;

(* Les  objets principaux  que je vais  manipuler  dans l'interpréteur  sont *
 * les 'bindings'.  Ils représentent  un  lien entre  un nom  (de type name) *
 * et quelque chose d'autre censé représenter ce qu'est ce nom. Par exemple, *
 * le binding le plus courant est Val_binding,  qui représente un lien entre *
 * un nom et une valeur.  Cette valeur peut être un Val_int  (qui représente *
 * un entier),  ou un Val_fun  (qui représente  la clôture d'une  fonction). *
 *                                                                           *
 * Dans Val_fun,  name est le nom de la variable muette qui est le paramètre *
 * de cette  fonction ;  expr est  l'expression  de cette fonction ;  et env *
 * est   l'environnement   dans  lequel   a   été   créée   cette  fonction. *
 *                                                                           *
 * Sur le  même principe  que Val_binding,  Ref_binding  représente  un lien *
 * entre un nom et une référence de valeur.                                  *
 *                                                                           *
 * En revanche,  Raw_binding fonctionne  un peu différemment.  Comme son nom *
 * l'indique,  il représente un lien entre nom et quelque chose d'autre, qui *
 * représente le minimum  nécessaire pour pouvoir  traiter ce Raw_binding au *
 * moment qui convient  (c'est  un peu  une sorte  d'évaluation paresseuse). *
 * Ainsi,  expr est l'expression  qui représente ce Raw_binding,  et env est *
 * l'environnement dans lequel ce Raw_binding a été défini.                  *
 *                                                                           *
 * Les Raw_binding  permettent de traiter  le cas des  fonctions récursives. *
 *)

type value =
  |Val_int  of int
  |Val_bool of bool
  |Val_fun  of pattern * expr * env
  |Val_err  of int
  |Val_ref  of value ref
  |Val_duo  of value * value
and binding =
  |Val_binding of pattern * value * mark
  |Raw_binding of pattern * expr  * env * mark
and env = binding list (* Que des Pat_name dans l'env *)
;;

let string_of_pattern = function
  |Pat_name n -> string_of_name n
  |Anonymous  -> "_"
  |_ -> failwith "Not a Pat_name"
;;

(* Permet de trouver un binding représentant name dans env *)
let rec env_lookup: env -> name -> binding = fun env x -> match env with
  |[]   -> failwith ("No " ^ (string_of_name x) ^ " in env")
  |(Val_binding(Pat_name n,_,_) as b)::s
  |(Raw_binding(Pat_name n,_,_,_) as b)::s -> if n = x then b else env_lookup s x
  |_ -> failwith "Pas de couple pour les let rec (comme en Caml)"
;;

(* Ajoute tous les bindings de binding_list à env *)
let rec env_push: env -> binding list -> env = fun env -> function
  |[] -> env
  |b::s -> b::(env_push env s)
;;

(* Renvoie un environnement*)
let empty_env: unit -> env = function () -> [];;

(* Convertit les entiers  en booléens,  et réciproquement. *
 * Dans mon interpréteur fouine, il n'y a pas de booléens. *
 * Ainsi, 0 équivaut à faux ; et tous les entiers non-nuls *
 * équivallent à vrai.                                     *
 *)

(* Interprète les opérations binaires *)
let do_op c a b =
  let f = fun x -> Val_int x in
  let g = function Val_int x -> x | _ -> raise Type_error in
  let h = fun x -> Val_bool x in
  let k = function Val_bool x -> x | _ -> raise Type_error in
  match c with
  |'+' -> f (g a + g b), INT
  |'-' -> f (g a - g b), INT
  |'*' -> f (g a * g b), INT
  |'/' -> f (g a / g b), INT
  |'<' -> h (g a < g b), BOOL
  |'>' -> h (g a > g b), BOOL
  |'=' -> h (g a = g b), BOOL
  |'l' -> h (g a <= g b), BOOL
  |'g' -> h (g a >= g b), BOOL
  |'#' -> h (g a <> g b), BOOL
  |'&' -> h (k a && k b), BOOL
  |'|' -> h (k a || k b), BOOL
  |_   -> failwith "unknow char"
;;

(* Fixe la pattern d'un 'binding' à 'pat' *)
let set_pattern pat = function 
  |Val_binding(_,v,m) -> Val_binding(pat,v,m)
  |Raw_binding(_,e,f,m) -> Raw_binding(pat,e,f,m)
;;

(* Fixe la pattern d'un 'binding' à 'pat' *)
let set_mark m = function 
  |Val_binding(pat,v,_)   -> Val_binding(pat,v,m)
  |Raw_binding(pat,e,f,_) -> Raw_binding(pat,e,f,m)
;;

let unifier_mark mark = function 
  |Val_binding(pat,v,m) -> Val_binding(pat,v,unification m mark)
  |Raw_binding(pat,e,f,m) -> Raw_binding(pat,e,f,unification m mark)
;;


(* Récupère le nom d'un binding *)
let name_of_binding = function 
  |Val_binding(Pat_name n,_,_)
  |Raw_binding(Pat_name n,_,_,_) -> n
  |_ -> failwith "Nom non récupérable depuis un binding"
;;

(* Récupère le nom d'un binding et le convertit en string *)
let string_of_binding b = (string_of_name (name_of_binding b));;

let print_err n = print_string "E "; print_int n;;

(* Renvoie le type d'une value, sous forme de string 
 * (fonction utilisée pour l'affichage)
 *)
let rec print_mark_value = function
  |Val_int _    -> print_string "int"
  |Val_bool _   -> print_string "bool"
  |Val_fun _    -> print_string "fun"
  |Val_err _    -> print_string "err"
  |Val_ref v    -> print_mark_value !v; print_string " ref"
  |Val_duo(v,w) -> print_mark_value v; print_string " * "; print_mark_value w
;;

(* Affiche la valeur d'une value *)
let rec print_value: value -> unit = function
  |Val_int n      -> print_int n
  |Val_bool b     -> print_string (if b then "true" else "false")
  |Val_fun(p,e,_) -> print_expr (Fun(p,e))
  |Val_err n      -> print_err n
  |Val_ref v      -> print_string "{content = "; print_value (!v); print_string "}"
  |Val_duo(v,w)   -> print_string "("; print_value v; print_string ", ";
    print_value w; print_string ")"
;;

(* Récupère la valeur d'un Val_binding *)
let content_of_val_bind = function
  |Val_binding(n,v,m) -> (n,v,m)
  |_ -> failwith "Not concerned"
;;

(* Renvoie vrai si le Val_binding est une erreur *)
let est_erreur = function
  |Val_binding(_,Val_err _,_) -> true
  |_                          -> false
;;

let rec list_make n a = match n with
  |m when n<0 -> failwith "Must be a positif integer"
  |0 -> []
  |_ -> a::(list_make (n-1) a)
;;

let rec ae_ajouter_mark_aux : pattern -> pattern * int = function
  |Pat_name _ as p
  |(Anonymous as p) -> (p,0)
  |Pat_mark(p,m)    -> let (p',n) = ae_ajouter_mark_aux p in
		       (Pat_mark(p',mark_of_mark_list m (list_make n PRIME)),n)
  |Pat_duo (p,q)    -> let (q',_) = ae_ajouter_mark_aux q in
		       let (p',_) = ae_ajouter_mark_aux p in
		       (Pat_duo(p',q'), 0)
  |Pat_fun pl as p  -> match pl with
    |[]
    |[_] -> failwith "Not possible, |pl| >= 2"
    |_   -> (p,List.length pl - 1)


let rec ae_enlever_mark : pattern -> pattern * mark = function
  |Pat_name _ as p
  |(Anonymous as p) -> (p, PRIME)
  |Pat_mark(p,m)    -> let (p',m') = ae_enlever_mark p in (p', unification m m')
  |Pat_duo (p,q)    -> let (q',qm) = ae_enlever_mark q in
		       let (p',pm) = ae_enlever_mark p in
		       (Pat_duo(p',q'), DUO(pm,qm))
  |Pat_fun pl       -> match pl with
    |[]
    |[_]   -> failwith "Not possible, |pl| >= 2"
    |f::ml -> let (f',fm)   = ae_enlever_mark f in
	      let (ml',mlm) = ae_enlever_mark_list ml in
	      (Pat_fun(f'::ml'), unification fm (mark_of_mark_list PRIME mlm))

and ae_enlever_mark_list : pattern list -> pattern list * mark list = function
  |[]   -> [], []
  |p::s -> let (p',m') = ae_enlever_mark p in
	   let (pl,ml) = ae_enlever_mark_list s in
	   (p'::pl,m'::ml)
;;

let ae_enlever_mark p = let p,_ = ae_ajouter_mark_aux p in ae_enlever_mark p;;

let ae_enlever_fun : pattern * expr -> pattern * expr = fun (p,e) -> match p with
  |Pat_name _
  |Pat_duo _
  |Anonymous  -> (p,e)
  |Pat_mark _ -> failwith "ae_enlever_mark doit être appliquée avant"
  |Pat_fun pl -> match pl with
    |[]
    |[_]    -> failwith "Not possible, |pl| >= 2"
    |f::[x] -> (f,Fun(x,e))
    |f::arg -> (f,Fun(Pat_fun arg,e))
;;

let rec bind_list_of_bind : binding -> binding list = function
  |Val_binding(Pat_duo(p,q), Val_duo(v,w), DUO(m,n)) ->
     let dl = bind_list_of_bind (Val_binding(q,w,n)) in
     let bl = bind_list_of_bind (Val_binding(p,v,m)) in
     dl @ bl
  |Val_binding(Pat_duo _, _, _) -> failwith "Why are you doing this to me?"
  |b -> [b]
;;



let rec fun_enlever_mark : pattern -> pattern * mark = function
  |Pat_name _ as p
  |(Anonymous as p) -> (p, PRIME)
  |Pat_mark(p,m)    -> let (p',m') = fun_enlever_mark p in (p', unification m m')
  |Pat_duo (p,q)    -> let (q',qm) = fun_enlever_mark q in
		       let (p',pm) = fun_enlever_mark p in
		       (Pat_duo(p',q'), DUO(pm,qm))
  |Pat_fun pl       -> match pl with
    |[]
    |[_] -> failwith "Not possible, |pl| >= 2"
    |ml  -> let (ml',mlm) = fun_enlever_mark_list ml in
	    (Pat_fun(ml'), mark_of_mark_list PRIME mlm)

and fun_enlever_mark_list : pattern list -> pattern list * mark list = function
  |[]   -> [], []
  |p::s -> let (p',m') = fun_enlever_mark p in
	   let (pl,ml) = fun_enlever_mark_list s in
	   (p'::pl,m'::ml)
;;


let fun_enlever_fun : pattern * expr -> pattern * expr = fun (p,e) -> match p with
  |Pat_name _ -> (p,e)
  |Pat_duo _
  |Anonymous
  |Pat_mark _ -> failwith "I want to have fun!"
  |Pat_fun pl -> match pl with
    |[]
    |[_]    -> failwith "Not possible, |pl| >= 2"
    |x::[y] -> (x,Fun(y,e))
    |x::arg -> (x,Fun(Pat_fun arg,e))
;;


(*
ae_enlever_mark (Pat_mark(Pat_fun
			    [
			      Pat_name (Name "f") ;
			      Pat_mark((Pat_name (Name "x")), INT);
			    ],
			  FUN(PRIME,PRIME)));;
*)
