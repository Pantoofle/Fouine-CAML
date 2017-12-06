%{open Types %}
   
%token APPLY PAT_LIST
%token DEF_LET DEF_IN DEF_FUN DEF_REC
%token COND_IF COND_THEN COND_ELSE
%token PROG_SEQ
%token MARK_DEF MARK_INT MARK_BOOL
%token EGAL FLECHE WITH VIRGULE
%token BOOL_ET BOOL_OU BOOL_NON
%token EXPR_PO EXPR_PF EXPR_SEQ
%token ERR_E ERR_RAISE ERR_TRY
%token REF DEREF ASSIGN
%token FEEDBACK PRINT
%token <int>    CONST_INT
%token <char>   COMP
%token <char>   OP_FOIS
%token <char>   OP_PLUS
%token <string> NAME
%token EOF


%nonassoc MARK_DEF
%right PAT_LIST

%left PROG_SEQ
%nonassoc ERR_TRY
%nonassoc WITH
%nonassoc DEF_LET
%nonassoc DEF_IN
%nonassoc DEF_FUN
%left EXPR_SEQ
%nonassoc COND_IF
%nonassoc COND_THEN
%nonassoc COND_ELSE
%right FLECHE
%nonassoc VIRGULE
%nonassoc ASSIGN
%left BOOL_ET
%left BOOL_OU
%left BOOL_NON
%nonassoc COMP
%nonassoc EGAL
%nonassoc OP_UNAIRE
%left OP_PLUS
%left OP_FOIS
%nonassoc EXPR_PO
%left FEEDBACK
%left PRINT
%left ERR_RAISE
%nonassoc NAME
%nonassoc CONST_INT
%left APPLY
%nonassoc ERR_E
%nonassoc REF
%nonassoc DEREF

%start main

%type <Types.prog> main

%%
  
  main:
   |prog PROG_SEQ EOF {$1}
  ;
  
  prog:
    
   /*|DEF_LET let_ass    {Prog_Let $2}*/
   |prog PROG_SEQ prog {Prog_Seq($1,$3)}
   |expr               {Prog_Expr $1}
  ;
  
  expr:
   |EXPR_PO expr EXPR_PF         {$2}
   |expr VIRGULE expr            {Duo($1,$3)}
   |CONST_INT                    {Int $1}
   |expr OP_PLUS expr            {Op($2,$1,$3)}
   |expr OP_FOIS expr            {Op($2,$1,$3)}
   |expr COMP expr               {Op($2,$1,$3)}
   |expr EGAL expr               {Op('=',$1,$3)}
   |expr BOOL_OU expr            {Op('|',$1,$3)}
   |expr BOOL_ET expr            {Op('&',$1,$3)}
   |BOOL_NON expr                {Not $2}
   |DEF_LET         pattern EGAL expr DEF_IN expr {Let(Ass($2,$4),$6)}
   |DEF_LET DEF_REC pattern EGAL expr DEF_IN expr {Rec(Ass($3,$5),$7)}
   |expr expr %prec APPLY        {Apply($1,$2)}
   |DEREF expr                   {Deref $2}
   |REF expr                     {Ref $2}
   |FEEDBACK expr                {Feedback $2}
   |PRINT expr                   {Print $2}
   |expr ASSIGN expr             {Assign($1, $3)}
   |NAME                         {Var(name_of_string $1)}
   |expr EXPR_SEQ expr           {Seq($1,$3)}
   |DEF_FUN pattern FLECHE expr  {Fun($2,$4)}
   |COND_IF expr COND_THEN expr COND_ELSE expr    {Ifte($2,$4,$6)}
   |ERR_RAISE ERR_E expr         {Raise $3}
   |ERR_RAISE EXPR_PO ERR_E expr EXPR_PF          {Raise $4}
   |ERR_TRY expr WITH ERR_E pattern FLECHE expr   {Try($2,Ass($5,$7))}
     /*
   |OP_PLUS expr %prec OP_UNAIRE {Op($1,Int 0, $2)}
     */
  ;

  pattern:
   |EXPR_PO pattern EXPR_PF {$2}    
   |NAME {if $1 = "_" then Anonymous else Pat_name (name_of_string $1)}
   |pattern pattern %prec PAT_LIST {
     match ($1,$2) with
     |(Pat_fun pl, Pat_fun ql) -> Pat_fun (pl @ ql)
     |(p, Pat_fun pl)          -> Pat_fun (p :: pl)
     |(Pat_fun pl, p)          -> failwith "with good precedence, shouldn't happen"
     |(p,q)                    -> Pat_fun [p;q]
   }
   |pattern VIRGULE pattern {Pat_duo ($1,$3)}
   |pattern MARK_DEF mark   {Pat_mark($1,$3)}
  ;

  mark:
   |EXPR_PO mark EXPR_PF {$2}    
   |MARK_INT             {INT}
   |MARK_BOOL            {BOOL}
   |mark FLECHE mark     {FUN($1,$3)}
   |mark REF             {REF $1}
   |mark OP_FOIS mark    {DUO($1,$3)}
