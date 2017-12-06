open Types
open Machine_utils
open Machine_types

let rec push_grabs c n =
  match n with
  | 0 -> c
  | _ -> GRAB::(push_grabs c (n-1))
		 
(* La fonction de traduction classique (Notée C dans la documentation) *)
let rec empile_args c start =
  match c with
  | Apply(a, b) -> empile_args a (compile_C b start) 
  | _ -> compile_C c start 
and compile_C tree code = 
  match tree with
  | Int(x)               -> (CONST_INT x)::code
  | Duo(x, y)            -> DUO::(compile_C y (compile_C x code))
  | Ref(c1)              -> REF::(compile_C c1 code)
  | Op(op, c1, c2)       -> OP(op)::(compile_C c2 (compile_C c1 code))
  | Let(a, ex)           -> ENDLET::(compile_C ex (GRAB::(compile_A a code)))
  | Rec(a, ex)           -> ENDLET::(compile_C ex (GRAB::(compile_AR a code)))
  (* ENDLET::(compile_C ex (GRAB::(CLOS_R(GRAB::(reverse(compile_T ex_f [])))::code))) *)
  | Fun(_, ex)           -> CLOS(reverse (compile_T tree []))::code
  | Var(Name nom)        -> ACCESS(int_of_string nom)::code
  | Apply(c1, c2)        -> APPLY::(empile_args tree (PUSHMARK::code))
  | Seq(c1, c2)          -> compile_C c2 (SEQ::(compile_C c1 code))
  | Ifte(cond, c1, c2)   -> ENDIF::(compile_C c2 (ELSE::(compile_C c1 (IF::(compile_C cond code)))))
  | Deref(c1)            -> DEREF::(compile_C c1 code)
  | Assign(x, y)         -> ASSIGN::(compile_C x (compile_C y code))
  | Not(c1)              -> NOT::(compile_C c1 code)
  | Raise(c1)            -> RAISE::(compile_C c1 code)
  | Try(c1, Ass(p, c2))  -> ENDTRY::(compile_C c1 (TRY::(compile_C (Fun(p, c2)) code)))
  | Print(c1)            -> PRINT::(compile_C c1 code)
  | Feedback(c1)         -> FEEDBACK::(compile_C c1 code)
  | _-> failwith "Wrong format during compilation"
and compile_A (a : ass) code =
  let Ass(p, e) = a in
  match p, e with
  | Pat_name(_), _    -> compile_C e code
  | Pat_mark(_, _), _ -> compile_C e code
  | Pat_duo(_, _), Duo(x, y) -> compile_C y (GRAB::(compile_C x code))
  | Pat_fun(pl), _  -> CLOS(push_grabs (reverse (compile_T e [])) ((List.length pl)-1))::code
  | _ -> failwith "Void assignement"
and compile_AR (a : ass) code =
  let Ass(p, e) = a in
  match p, e with
  | Pat_name(_), _    -> compile_C e code
  | Pat_mark(_, _), _ -> compile_C e code
  | Pat_duo(_, _), Duo(x, y) -> compile_C y (GRAB::(compile_C x code))
  | Pat_fun(pl), _  -> CLOS_R(push_grabs (reverse(compile_T e [])) ((List.length pl)-1))::code
  | _ -> failwith "Void assignement"
and compile_T tree code =
  match tree with
  | Int(x)               -> RETURN::(CONST_INT x)::code
  | Duo(x, y)            -> RETURN::DUO::(compile_C y (compile_C x code))
  | Op(op, c1, c2)       -> RETURN::OP(op)::(compile_C c2 (compile_C c1 code))
  | Let(a, ex)           -> RETURN::ENDLET::(compile_C ex (GRAB::(compile_A a code)))
  | Rec(a, ex)           -> RETURN::ENDLET::(compile_C ex (GRAB::(compile_AR a code)))
  | Fun(_, ex)           -> compile_T ex (GRAB::code)
  | Var(Name nom)        -> RETURN::ACCESS(int_of_string nom)::code
  | Apply(c1, c2)        -> TAILAPPLY::(empile_args tree code)
  | Seq(c1, c2)          -> RETURN::compile_C c2 (SEQ::(compile_C c1 code))
  | Ifte(cond, c1, c2)   ->
     RETURN::ENDIF::(compile_C c2 (ELSE::(compile_C c1 (IF::(compile_C cond code)))))
  | Deref(c1)            -> RETURN::DEREF::(compile_C c1 code)
  | Assign(x, y)         -> RETURN::ASSIGN::(compile_C x (compile_C y code))
  | Ref(c1)              -> RETURN::REF::(compile_C c1 code)
  | Not(c1)              -> RETURN::NOT::(compile_C c1 code)
  | Raise(c1)            -> RETURN::RAISE::(compile_C c1 code)
  | Try(c1, Ass(p, c2))  -> RETURN::ENDTRY::(compile_C c1 (TRY::(compile_C (Fun(p, c2)) code)))
  | Print(c1)            -> RETURN::PRINT::(compile_C c1 code)
  | Feedback(c1)         -> RETURN::FEEDBACK::(compile_C c1 code)
  | _-> failwith "Wrong format during compilation"
       
let compiler ex =
  let nexp = De_bruijn.tree_to_bruijn ex [] in
  reverse (compile_C nexp [])

