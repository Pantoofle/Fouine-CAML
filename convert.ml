(* Prend une in_channel et renvoie le programme parsé correspondant *)
let prog_of_in_channel in_channel = Parser.main Lexer.token (Lexing.from_channel in_channel);;

open Types;;

let rec search_expr: name -> expr list -> expr = fun name -> function
  |[] -> failwith "Pas de tel expression dans cette liste"
  |Let(Ass(Pat_name n,(Fun _ as f)),_)::s ->
     if n=name then f else search_expr name s
  |_::s -> search_expr name s
;;

let rec replace_pattern : (name -> name) -> pattern -> pattern =
  fun r_name -> function
  |Pat_name name -> Pat_name (r_name name)
  |Pat_fun pl    -> Pat_fun (List.map (replace_pattern r_name) pl)
  |Pat_mark(p,m) -> Pat_mark(replace_pattern r_name p,m)
  |Pat_duo (p,q) -> Pat_duo (replace_pattern r_name p,replace_pattern r_name q)
  |Anonymous     -> Anonymous
;;

let rec replace_expr = fun (expr : expr) -> fun
  (r_expr : expr -> expr) (r_int : int -> int)
  (r_name : name -> name) (r_op : char -> char) ->
    let r_e e = replace_expr e r_expr r_int r_name r_op
    and r_a a = replace_ass  a r_expr r_int r_name r_op
    and r_p p = replace_pattern r_name p in
    let expr = r_expr expr in match expr with
      |Int n -> Int(r_int n)
      |Ref e -> Ref(r_e e)
      |Duo(e1,e2)  -> Duo(r_e e1, r_e e2)
      |Op(c,e1,e2) -> Op(r_op c, r_e e1, r_e e2)
      |Not e -> Not (r_e e)
      |Let(a,e) -> Let(r_a a, r_e e)
      |Rec(a,e) -> Rec(r_a a, r_e e)
      |Fun(p,e) -> Fun(r_p p, r_e e)
      |Var n -> Var(r_name n)
      |Deref e -> Deref (r_e e)
      |Assign(f,e) -> Assign(r_e f, r_e e)
      |Apply(e1,e2) -> Apply(r_e e1, r_e e2)
      |Seq(e1,e2) -> Seq(r_e e1, r_e e2)
      |Feedback e -> Feedback (r_e e)
      |Print e -> Print (r_e e)
      |Ifte(e1,e2,e3) -> Ifte(r_e e1, r_e e2, r_e e3)
      |Raise e -> Raise(r_e e)
      |Try(e,a) -> Try(r_e e, r_a a)
and replace_ass = fun (Ass(p,e) : ass) -> fun
  (r_expr : expr -> expr) (r_int : int -> int)
  (r_name : name -> name) (r_op : char -> char) ->
    let r_e e = replace_expr e r_expr r_int r_name r_op in
    Ass(replace_pattern r_name p,r_e e)
;;

(* _e1 -> 0 ; _e2 -> 1 ; ... ; _e9 -> 8 *)
let int_of_underscore s = int_of_string (String.make 1 s.[2]) - 1 ;;

let rec convert: expr list -> expr -> expr = fun el expr ->
  let id = fun x -> x
  and und (l:expr array) = function
    |Var (Underscore s) -> l.(int_of_underscore s)
    |e -> e
  and ch c' = fun c -> if c = '_' then c' else c
  and nm n' = fun n -> if n = Underscore "_x" then n' else n in      
  let r str r_expr r_int r_name r_op =
    replace_expr (search_expr (Name str) el) r_expr r_int r_name r_op
  in match expr with    
  |Int n ->
     r "t_int" (und [|expr|]) id id id 
  |Op(c,e1,e2) ->
     let e1 = convert el e1 and e2 = convert el e2 in
     r "t_op" (und [|e1; e2|]) id id (ch c)
  |Not e ->
     let e = convert el e in
     r "t_not" (und [|e|]) id id id
  |Let(Ass(Pat_name n,e1),e2) ->
     let e1 = convert el e1 and e2 = convert el e2 in
     r "t_let" (und [|e1; e2|]) id (nm n) id
  |Rec(Ass(Pat_name n,e1),e2) ->
     let e1 = convert el e1 and e2 = convert el e2 in
     r "t_rec" (und [|e1; e2|]) id (nm n) id
  |Fun(Pat_name n,e) ->
     let e = convert el e in
     r "t_fun" (und [|e|]) id (nm n) id
  |Var x -> r "t_var" (und [|expr|]) id id id
  |Apply(e1,e2) ->
     let e1 = convert el e1 and e2 = convert el e2 in
     r "t_app" (und [|e1; e2|]) id id id
  |Seq(e1,e2) ->
     let e1 = convert el e1 and e2 = convert el e2 in
     r "t_seq" (und [|e1; e2|]) id id id
  |Ifte(e1,e2,e3) ->
     let e1 = convert el e1 and e2 = convert el e2 and e3 = convert el e3 in
     r "t_ifte" (und [|e1; e2; e3|]) id id id
  |Raise e ->
     let e = convert el e in
     r "t_raise" (und [|e|]) id id id
  |Try(e1,Ass(Pat_name n,e2)) ->
     let e1 = convert el e1 and e2 = convert el e2 in
     r "t_try" (und [|e1; e2|]) id (nm n) id
  |_ -> failwith "WIP"
;;

(* Convertit une expression de Caml + blabla en Caml, en utilsant la *
 * fonction "use" du fichier fouine spécial, de façon à avoir le bon *
 * type à la fin de la conversion.                                   *
 *)

let convert_use : expr list -> expr -> expr = fun el e ->
  let e = convert el e in
  Apply(search_expr (Name "use") el, e)
;;

let name_of_int = function
  |1 -> "exception.fou.ml"
  |2 -> failwith "recursion WIP"
  |3 -> failwith "WIP"
  |_ -> failwith "This is not possible"
;;

(* n faut 1, 2 ou 3, et désigne la transformation que l'on veut faire. *)
let transformation n expr =
  let file = open_in (name_of_int n) in
  let result = prog_of_in_channel file in
  let transfos = ref [] in
  let () = prog_map (fun e -> transfos := e :: !transfos) result in
  let expressions = !transfos in
  convert_use expressions expr
;;
