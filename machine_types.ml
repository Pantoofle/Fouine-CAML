(* Les lexems à utiliser par la machine *)
type lexem =
  | ACCESS     of int
  | GRAB
  | ASSIGN
  | CLOS       of code
  | CLOS_R     of code
  | CONST_INT  of int
  | OP         of char
  | DUO
  | SEQ
  | TRY      
  | ENDTRY
  | RAISE
  | REF        
  | DEREF
  | APPLY
  | TAILAPPLY
  | PUSHMARK
  | NOT
  | PRINT
  | FEEDBACK
  | ENDLET
  | IF
  | ELSE
  | ENDIF
  | RETURN
 and code = lexem list

(* Les types utilisés dans la pile et l'environement *)
type value =
  | VAL_INT of int
  | VAL_BOOL of bool
  | VAL_FUN of code * env
  | VAL_REC of code * env
  | VAL_DUO of value * value
  | VAL_REF of value ref
 and stack_entry =
   | S_VAL of value
   | S_ENV of env
   | S_CODE of code
   | S_MARK
 and env = value list
 and stack = stack_entry list ref
 and z_error_caller = (code * code * env * stack * stack) list ref
 and error_caller = (code * code * env) list ref
