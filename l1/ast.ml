
type var = string 

type oper = ADD | GTEQ


type 'a expr = 
  | Unit of 'a
  | Var of 'a * var
  | Integer of 'a * int
  | Boolean of 'a * bool
  | Op of 'a * 'a expr * oper * 'a expr
  | If of 'a * 'a expr * 'a expr * 'a expr
  | While of 'a * 'a expr * 'a expr
  | Deref of 'a * 'a expr
  | Assign of 'a * 'a expr * 'a expr
  | Lambda of 'a lambda
  | App of 'a * 'a expr * 'a expr
  | LetFun of 'a * var * 'a lambda * 'a expr
  | LetRecFun of 'a * var * 'a lambda * 'a expr


and lambda = var * expr 


open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 



let pp_bop = function 
  | ADD -> "+" 
  | GTEQ -> ">="
  | EQ -> "="


let string_of_oper = pp_bop 

let fstring ppf s = fprintf ppf "%s" s

let pp_unary ppf t = fstring ppf (pp_uop t) 

let pp_binary ppf t = fstring ppf (pp_bop t) 

let rec pp_expr ppf = function 
    | Unit _               -> fstring ppf "()"
    | Var(_, x)            -> fstring ppf x
    | Integer(_, n)        -> fstring ppf (string_of_int n)
    | Boolean(_, b)        -> fstring ppf (string_of_bool b)
    | Op(_, e1, op, e2)    -> fprintf ppf "(%a %a %a)" pp_expr e1  pp_binary op pp_expr e2
    | If(_, e1, e2, e3)    -> fprintf ppf "@[if %a then %a else %a @]"
                                      pp_expr e1 pp_expr e2 pp_expr e3
    | Lambda(_, x, e) ->
        fprintf ppf "(fun %a -> %a)" fstring x pp_expr e
    | App(_, e1, e2)       -> fprintf ppf "%a %a" pp_expr e1 pp_expr e2

    | While(_, e1, e2)     -> fprintf ppf "while %a do %a end" pp_expr e1 pp_expr e2
    | Deref(_, e)          -> fprintf ppf "!(%a)" pp_expr e
    | Assign(_, e1, e2)    -> fprintf ppf "(%a := %a)" pp_expr e1 pp_expr e2
    | LetFun(_, f, (_, x, e1), e2)     ->
        fprintf ppf "@[let %a(%a) =@ %a @ in %a @ end@]"
                    fstring f fstring x  pp_expr e1 pp_expr e2
    | LetRecFun(_, f, (_, x, e1), e2)  ->
        fprintf ppf "@[letrec %a(%a) =@ %a @ in %a @ end@]"
                    fstring f fstring x  pp_expr e1 pp_expr e2
	
and pp_expr_list ppf = function 
  | [] -> () 
  | [e] -> pp_expr ppf e 
  |  e:: rest -> fprintf ppf "%a; %a" pp_expr e pp_expr_list rest 


let print_expr e = 
    let _ = pp_expr std_formatter e
    in print_flush () 

let eprint_expr e = 
    let _ = pp_expr err_formatter e
    in pp_print_flush err_formatter () 

(* useful for debugging *) 

let string_of_uop = function 
  | NEG -> "NEG" 

let string_of_bop = function 
  | ADD -> "ADD" 
  | GTEQ -> "GTEQ"
  | EQ -> "EQ"

let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_expr = function 
    | Unit _            -> "Unit"
    | Var(_, x)            -> mk_con "Var" [x]
    | Integer(_, n)        -> mk_con "Integer" [string_of_int n]
    | Boolean(_, b)        -> mk_con "Boolean" [string_of_bool b]
    | UnaryOp(_, op, e)   -> mk_con "UnaryOp" [string_of_uop op; string_of_expr e]
    | Op(_, e1, op, e2)   -> mk_con "Op" [string_of_expr e1; string_of_bop op; string_of_expr e2]
    | If(_, e1, e2, e3)   -> mk_con "If" [string_of_expr e1; string_of_expr e2; string_of_expr e3]
    | Pair(_, e1, e2)     -> mk_con "Pair" [string_of_expr e1; string_of_expr e2]
    | Fst(_, e)            -> mk_con "Fst" [string_of_expr e]
    | Snd(_, e)            -> mk_con "Snd" [string_of_expr e]
    | Inl(_, e)            -> mk_con "Inl" [string_of_expr e]
    | Inr(_, e)            -> mk_con "Inr" [string_of_expr e]
    | Lambda(_, x, e)     -> mk_con "Lambda" [x; string_of_expr e]
    | App(_, e1, e2)      -> mk_con "App" [string_of_expr e1; string_of_expr e2]
    | Seq(_, el)           -> mk_con "Seq" [string_of_expr_list el]
    | While(_, e1, e2)   -> mk_con "While" [string_of_expr e1; string_of_expr e2]
    | Ref(_, e)            -> mk_con "Ref" [string_of_expr e]
    | Deref(_, e)          -> mk_con "Deref" [string_of_expr e]
    | Assign (_, e1, e2)  -> mk_con "Assign" [string_of_expr e1; string_of_expr e2]
    | LetFun(_, f, (_, x, e1), e2)      ->
          mk_con "LetFun" [f; mk_con "" [x; string_of_expr e1]; string_of_expr e2]
    | LetRecFun(_, f, (_, x, e1), e2)   ->
          mk_con "LetRecFun" [f; mk_con "" [x; string_of_expr e1]; string_of_expr e2]

and string_of_expr_list = function 
  | [] -> "" 
  | [e] -> string_of_expr e 
  |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)

