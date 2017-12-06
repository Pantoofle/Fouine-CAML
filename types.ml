type name =
  |Name       of string
  |Underscore of string (* Nom commançant par '_', autorisé que dans mes fichiers *)
  |Reserved   of string (* Nom réservé (comme prInt et feedback) *)
;;

type mark =
  |INT
  |BOOL
  |FUN of mark * mark
  |REF of mark
  |DUO of mark * mark
  |PRIME
;;

type pattern =
  |Pat_name of name
  |Pat_fun  of pattern list
  |Pat_mark of pattern * mark
  |Pat_duo  of pattern * pattern
  |Anonymous
;;
  
type expr =
  |Int      of int
  |Duo      of expr * expr
  |Ref      of expr
  |Op       of char * expr * expr (* Op représente les opérations.
				   * char vaut '+', '-', '*', '/',
				   * '&', '|', '=', '#', '<', '>', 'l', 'g'
				   * char peut valoir '_',
				   * qui est l'opérateur réservé
				   *)
  |Not      of expr
  |Let      of ass * expr (* assignement ass dans expr *)
  |Rec      of ass * expr
  |Fun      of pattern * expr (* fun pattern -> expr *)
  |Var      of name
  |Deref    of expr
  |Assign   of expr * expr (* expr1 := expr2 *)
  |Apply    of expr * expr (* f 2 où name = f et expr = 2 *)
  |Seq      of expr * expr
  |Feedback of expr
  |Print    of expr
  |Ifte     of expr * expr * expr
  |Raise    of expr
  |Try      of expr * ass
and ass =
  |Ass of pattern * expr
;;

(*Transforme une chaîne de caractères en Name *)
let name_of_string str =
  if str = "" then failwith "Can't happen in my world"
  else if str.[0] = '_' then Underscore str
  else if str = "prInt" || str = "feedback" then Reserved str
  else Name str
;;
	
(* Pour l'instant, un prog est une suite de expr *)
type prog =
  |Prog_Let  of ass (* Non toléré pour l'instant *)
  |Prog_Seq  of prog * prog
  |Prog_Expr of expr
;;

(* Prend une fonction 'f' qui s'applique à une expr,
 * ainsi qu'un programme 'prog', et applique 'f' à chaque
 * expr de prog.
 *)
let rec prog_map (f:expr -> unit) = function
  |Prog_Let a    -> failwith "WIP"
  |Prog_Seq(p,q) -> prog_map f p; prog_map f q
  |Prog_Expr p   -> f p
;;

let string_of_name = function
  |Name s
  |Reserved s
  |Underscore s -> s
;;

let print_name n = print_string (string_of_name n);;

let print_op c = match c with
  |'+' | '-' | '*' | '/' | '=' | '<' | '>' -> print_char c
  |'&' -> print_string "&&"
  |'|' -> print_string "||"
  |'#' -> print_string "<>"
  |'l' -> print_string "<="
  |'g' -> print_string ">="
  |_   -> failwith "Impossible, scrogneugneu !"
;;

let rec print_mark = function
  |INT      -> print_string "int"
  |BOOL     -> print_string "bool"
  |FUN(n,m) -> print_string "("; print_mark n ; print_string " -> ";
    print_mark m; print_string ")"
  |REF m    -> print_string "("; print_mark m; print_string " ref)"
  |DUO(n,m) -> print_string "("; print_mark n ; print_string " * ";
    print_mark m; print_string ")"
  |PRIME    -> print_string "_"
;;

let rec print_pattern = function
  |Pat_name n    -> print_name n
  |Pat_fun pl    -> print_pattern_list pl
  (* pl de taille au moins 2, et sans Pat_fun*)
  |Pat_mark(p,m) -> print_string "("; print_pattern p; print_string " : ";
    print_mark m; print_string ")"
  |Pat_duo(p,q)  -> print_string "("; print_pattern p; print_string ", ";
    print_pattern q; print_string ")"
  |Anonymous     -> print_string "_"

and print_pattern_list = function
  |[]   -> failwith "empty pattern list"
  |[p]  -> print_pattern p
  |p::s -> print_pattern p ; print_char ' ' ; print_pattern_list s
;;

(* 'print_expr' affiche une expression *)
(* 'print_ass'  affiche un assignement *)
let rec print_expr = function
  |Int n       -> print_int n
  |Duo(e,f)    -> print_char '('; print_expr e;
		  print_string ", "; print_expr f; print_char ')'
  |Ref e       -> print_string "(ref "; print_expr e; print_char ')'
  |Op (c,a,b)  -> print_char '(' ; print_expr a ;
    print_char c ; print_expr b ;
    print_char ')'
  |Not e       -> print_string "(not "; print_expr e; print_char ')'
  |Let(a,e)    -> print_string "(let "; print_ass a;
    print_string " in "; print_expr e; print_char ')'
  |Rec(a,e)    -> print_string "(let rec "; print_ass a;
    print_string " in "; print_expr e; print_char ')'
  |Fun(n,e)    -> print_string "(fun "; print_pattern n;
    print_string " -> "; print_expr e; print_char ')'
  |Var n       -> print_name n
  |Apply(e,f)  -> print_char '('; print_expr e;
		  print_char ' '; print_expr f; print_char ')'
  |Seq(e,f)    -> print_char '('; print_expr e;
    print_string "; "; print_expr f; print_char ')'
  |Ifte(e,f,g) -> print_string "(if " ; print_expr e ;
    print_string " then " ; print_expr f ;
    print_string " else " ; print_expr g ;
    print_char ')'
  |Deref e      -> print_char '!'; print_expr e
  |Feedback e   -> print_string "(feedback "; print_expr e; print_char ')'
  |Print e      -> print_string "(prInt ";    print_expr e; print_char ')'
  |Assign(n, e) -> print_char '(';
		   print_expr n; print_string " := ";
		   print_expr e; print_char ')'
  |Raise(e)    -> print_string "raise (E "; print_expr e; print_char ')'
  |Try(e, a)   -> print_string "(try "; print_expr e; print_string " with E ";
		  print_ass a; print_char ')'
and print_ass = function
  |Ass(n,e)    -> print_pattern n; print_string " = "; print_expr e
;;

(* Fonction appelé dans le main.ml *)
let print_feedback p =
  print_expr p; print_newline ()
;;
