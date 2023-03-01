(*   translate_expr : Past.expr -> Ast.expr 
     
	 Lifted and amended from the original Slang interpreter

*) 

let translate_bop = function 
  | Past.ADD -> Ast.ADD 
  | Past.GTEQ -> Ast.GTEQ
  | Past.EQ -> Ast.EQ


let rec translate_expr = function 
    | Past.Integer(_, n)     -> Ast.Integer n
    | Past.Var(l, x)         -> Ast.Var(l, x)
    | Past.Integer(l, n)     -> Ast.Integer(l, n)
    | Past.Boolean(l, b)     -> Ast.Boolean(l, b)
    | Past.Op(_, e1, op, e2) -> Ast.Op(translate_expr e1, translate_bop op, translate_expr e2)
    | Past.If(l, e1, e2, e3) -> Ast.If(l, translate_expr e1, translate_expr e2, translate_expr e3)
    | Past.App(l, e1, e2)    -> Ast.App(l, translate_expr e1, translate_expr e2)
    | Past.Let(l, x, _, e1, e2) ->
         Ast.App(l, Ast.Lambda(l, x, translate_expr e2), translate_expr e1)
    | Past.LetFun(l, f, lam, _, e)     ->
         Ast.LetFun(l, f, translate_lambda l lam, translate_expr e)
    | Past.LetRecFun(l, f, lam, _, e)     ->
         Ast.LetRecFun(l, f, translate_lambda l lam, translate_expr e)
    | Past.While(l, e1, e2) -> Ast.While(l, translate_expr e1, translate_expr e2)
    | Past.Deref(l, e) -> Ast.Deref(l, translate_expr e)
    | Past.Assign(l, e1, e2) -> Ast.Assign(l, translate_expr e1, translate_expr e2)