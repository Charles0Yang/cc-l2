/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT
%token<string> IDENT
%token ADD SEMICOLON COLON GTEQ 
%token EQUAL ASSIGN DEREF ARROW 
%token IF THEN ELSE TRUE FALSE WHILE DO SKIP BOOL INTTYPE
%token LET IN FN VAL REC
%token LPAREN RPAREN
%token BEGIN END
%token EOF
%left ADD    /* lowest precedence */
%left EQUAL ARROW GTEQ    /* medium precedence */
%nonassoc INT IDENT TRUE FALSE LPAREN DEREF REF /* highest precedence */


%start main
%type <Past.expr> simple_expr 
%type <Past.expr> expr 
%type <Past.expr list> exprlist
%type <Past.expr> main

%%
main:
	expr EOF                { $1 }
;
simple_expr:
| INT                                { Past.Integer (get_loc(), $1) }
| IDENT                              { Past.Var (get_loc(), $1) }
| TRUE                               { Past.Boolean (get_loc(), true)}
| FALSE                              { Past.Boolean (get_loc(), false)}
| LPAREN expr RPAREN                 { $2 }
| DEREF simple_expr              	 { Past.Deref(get_loc(), $2) }
| REF simple_expr               	 { Past.Ref(get_loc(), $2) }

expr:
| simple_expr                        {  $1 }
| expr ADD expr                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr GTEQ expr                     { Past.Op(get_loc(), $1, Past.GTEQ, $3) }
| expr EQUAL expr                    { Past.Op(get_loc(), $1, Past.EQ, $3) }
| expr ASSIGN expr                   { Past.Assign(get_loc(), $1, $3) }
| BEGIN exprlist END                 { Past.Seq(get_loc(), $2) }
| IF expr THEN expr ELSE expr END    { Past.If(get_loc(), $2, $4, $6) }
| WHILE expr DO expr END             { Past.While(get_loc(), $2, $4) }
| BEGIN exprlist END                 { Past.Seq(get_loc(), $2) }
| FN LPAREN IDENT COLON texpr RPAREN ARROW expr END 
                                     { Past.Lambda(get_loc(), ($3, $5, $8)) } 
| LET IDENT COLON texpr EQUAL expr IN expr END           { Past.Let (get_loc(), $2, $4, $6, $8) }
| LET IDENT LPAREN IDENT COLON texpr RPAREN COLON texpr EQUAL expr IN expr END 
                                     { Past.LetFun (get_loc(), $2, ($4, $6, $11), $9, $13) }

exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }

texpr: 
| BOOL                               { Past.TEbool  }
| INTTYPE                            { Past.TEint  }
| texpr ARROW texpr                  { Past.TEarrow ($1, $3)}
| texpr ADD texpr                    { Past.TEunion ($1, $3)}
| LPAREN texpr RPAREN                { $2 } 


