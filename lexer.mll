{
  open Parser;;
}
    
rule token = parse
(* | "(*" ( [^ '*' '('] | ('*' [^ ')']) | ('(' [^ '*']) )* "*)" {token lexbuf} *)
| [' ' '\t' '\n'] {token lexbuf}
| ['0'-'9']+ as s {CONST_INT (int_of_string s)}
| ['+' '-']  as c {OP_PLUS c}
| ['*' '/']  as c {OP_FOIS c}
| ['<' '>']  as c {COMP c}
| "feedback"      {FEEDBACK}
| "_+"    {OP_PLUS '_'} 
| "<="    {COMP 'l'}
| ">="    {COMP 'g'}
| "<>"    {COMP '#'}
| "prInt" {PRINT}
| "int"   {MARK_INT}
| "bool"  {MARK_BOOL}
| "let"   {DEF_LET}
| "in"    {DEF_IN}
| "fun"   {DEF_FUN}
| "rec"   {DEF_REC}
| "ref"   {REF}
| "if"    {COND_IF}
| "then"  {COND_THEN}
| "else"  {COND_ELSE}
| "E"     {ERR_E}
| "raise" {ERR_RAISE}
| "try"   {ERR_TRY}
| "with"  {WITH}
| "not"   {BOOL_NON}
| "&&"    {BOOL_ET}
| "||"    {BOOL_OU}
| '='     {EGAL}
| '!'     {DEREF}
| ":="    {ASSIGN}
| "->"    {FLECHE}
| ','     {VIRGULE}
| ':'     {MARK_DEF}
| ";;"    {PROG_SEQ}
| '('     {EXPR_PO}
| ')'     {EXPR_PF}
| ';'     {EXPR_SEQ}
| ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as s {NAME s}
| eof     {EOF}
