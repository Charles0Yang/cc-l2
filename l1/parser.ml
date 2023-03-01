type token =
  | INT of (int)
  | IDENT of (string)
  | ADD
  | SEMICOLON
  | COLON
  | GTEQ
  | EQUAL
  | ASSIGN
  | DEREF
  | ARROW
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | WHILE
  | DO
  | SKIP
  | BOOL
  | INTTYPE
  | LET
  | IN
  | FN
  | VAL
  | REC
  | LPAREN
  | RPAREN
  | BEGIN
  | END
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"

let get_loc = Parsing.symbol_start_pos 

# 40 "parser.ml"
let yytransl_const = [|
  259 (* ADD *);
  260 (* SEMICOLON *);
  261 (* COLON *);
  262 (* GTEQ *);
  263 (* EQUAL *);
  264 (* ASSIGN *);
  265 (* DEREF *);
  266 (* ARROW *);
  267 (* IF *);
  268 (* THEN *);
  269 (* ELSE *);
  270 (* TRUE *);
  271 (* FALSE *);
  272 (* WHILE *);
  273 (* DO *);
  274 (* SKIP *);
  275 (* BOOL *);
  276 (* INTTYPE *);
  277 (* LET *);
  278 (* IN *);
  279 (* FN *);
  280 (* VAL *);
  281 (* REC *);
  282 (* LPAREN *);
  283 (* RPAREN *);
  284 (* BEGIN *);
  285 (* END *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\004\000\005\000\005\000\
\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\003\000\002\000\002\000\
\001\000\003\000\003\000\003\000\003\000\003\000\007\000\005\000\
\003\000\009\000\009\000\014\000\001\000\003\000\001\000\001\000\
\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\000\000\000\000\004\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\009\000\
\000\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\006\000\000\000\014\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\024\000\000\000\
\000\000\000\000\000\000\022\000\000\000\016\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\000\000\000\000\
\025\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\018\000\000\000\000\000\000\000\
\000\000\020\000"

let yydgoto = "\002\000\
\015\000\016\000\024\000\025\000\049\000"

let yysindex = "\255\255\
\041\255\000\000\000\000\000\000\044\255\041\255\000\000\000\000\
\041\255\002\255\249\254\041\255\041\255\044\255\000\000\000\000\
\003\000\000\000\142\255\121\255\034\255\025\255\099\255\157\255\
\000\255\000\000\041\255\041\255\041\255\041\255\000\000\041\255\
\041\255\113\255\045\255\046\255\000\000\041\255\000\000\102\255\
\058\255\058\255\163\255\134\255\004\255\000\000\000\000\113\255\
\148\255\063\255\113\255\000\000\041\255\000\000\021\255\113\255\
\041\255\113\255\113\255\022\255\015\255\000\000\065\255\108\255\
\000\000\091\255\066\255\000\000\041\255\068\255\041\255\075\255\
\113\255\084\255\000\000\149\255\000\000\041\255\114\255\041\255\
\090\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\051\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\001\000\028\000\054\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\255\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\058\000\008\000\046\000\213\255"

let yytablesize = 339
let yytable = "\001\000\
\011\000\026\000\031\000\021\000\055\000\026\000\027\000\060\000\
\017\000\028\000\029\000\030\000\063\000\019\000\065\000\066\000\
\020\000\027\000\022\000\023\000\028\000\029\000\030\000\056\000\
\056\000\026\000\036\000\012\000\039\000\076\000\058\000\058\000\
\054\000\010\000\040\000\041\000\042\000\043\000\034\000\044\000\
\045\000\003\000\004\000\068\000\003\000\004\000\050\000\062\000\
\067\000\005\000\051\000\006\000\005\000\013\000\007\000\008\000\
\009\000\007\000\008\000\035\000\061\000\010\000\018\000\011\000\
\064\000\030\000\012\000\059\000\013\000\012\000\014\000\026\000\
\073\000\014\000\058\000\071\000\072\000\027\000\074\000\021\000\
\028\000\029\000\030\000\052\000\000\000\079\000\027\000\081\000\
\000\000\028\000\029\000\030\000\027\000\056\000\000\000\028\000\
\029\000\030\000\000\000\000\000\058\000\027\000\000\000\075\000\
\028\000\029\000\030\000\028\000\029\000\030\000\027\000\000\000\
\077\000\028\000\029\000\030\000\027\000\070\000\082\000\028\000\
\029\000\030\000\000\000\027\000\000\000\037\000\028\000\029\000\
\030\000\069\000\000\000\046\000\047\000\000\000\000\000\080\000\
\027\000\033\000\048\000\028\000\029\000\030\000\000\000\000\000\
\027\000\000\000\053\000\028\000\029\000\030\000\056\000\056\000\
\000\000\032\000\057\000\078\000\000\000\058\000\058\000\027\000\
\038\000\000\000\028\000\029\000\030\000\027\000\000\000\000\000\
\028\000\029\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\000\011\000\027\000\011\000\011\000\
\028\000\029\000\030\000\000\000\011\000\011\000\000\000\000\000\
\000\000\011\000\000\000\000\000\000\000\000\000\011\000\000\000\
\000\000\000\000\000\000\011\000\000\000\011\000\012\000\012\000\
\000\000\012\000\012\000\000\000\010\000\010\000\000\000\012\000\
\012\000\000\000\000\000\000\000\012\000\010\000\010\000\000\000\
\000\000\012\000\010\000\000\000\000\000\000\000\012\000\010\000\
\012\000\013\000\000\000\000\000\010\000\000\000\010\000\000\000\
\000\000\013\000\013\000\000\000\000\000\000\000\013\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\013\000\000\000\013\000"

let yycheck = "\001\000\
\000\000\003\001\000\000\002\001\048\000\007\001\003\001\051\000\
\001\000\006\001\007\001\008\001\056\000\006\000\058\000\059\000\
\009\000\003\001\026\001\012\000\006\001\007\001\008\001\003\001\
\003\001\027\001\002\001\000\000\029\001\073\000\010\001\010\001\
\029\001\000\000\027\000\028\000\029\000\030\000\005\001\032\000\
\033\000\001\001\002\001\029\001\001\001\002\001\002\001\027\001\
\027\001\009\001\005\001\011\001\009\001\000\000\014\001\015\001\
\016\001\014\001\015\001\026\001\053\000\021\001\005\000\023\001\
\057\000\008\001\026\001\005\001\028\001\026\001\030\001\014\000\
\005\001\030\001\010\001\010\001\069\000\003\001\071\000\029\001\
\006\001\007\001\008\001\038\000\255\255\078\000\003\001\080\000\
\255\255\006\001\007\001\008\001\003\001\003\001\255\255\006\001\
\007\001\008\001\255\255\255\255\010\001\003\001\255\255\029\001\
\006\001\007\001\008\001\006\001\007\001\008\001\003\001\255\255\
\029\001\006\001\007\001\008\001\003\001\027\001\029\001\006\001\
\007\001\008\001\255\255\003\001\255\255\027\001\006\001\007\001\
\008\001\022\001\255\255\019\001\020\001\255\255\255\255\022\001\
\003\001\017\001\026\001\006\001\007\001\008\001\255\255\255\255\
\003\001\255\255\013\001\006\001\007\001\008\001\003\001\003\001\
\255\255\012\001\007\001\007\001\255\255\010\001\010\001\003\001\
\004\001\255\255\006\001\007\001\008\001\003\001\255\255\255\255\
\006\001\007\001\008\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\003\001\006\001\007\001\
\006\001\007\001\008\001\255\255\012\001\013\001\255\255\255\255\
\255\255\017\001\255\255\255\255\255\255\255\255\022\001\255\255\
\255\255\255\255\255\255\027\001\255\255\029\001\003\001\004\001\
\255\255\006\001\007\001\255\255\003\001\004\001\255\255\012\001\
\013\001\255\255\255\255\255\255\017\001\012\001\013\001\255\255\
\255\255\022\001\017\001\255\255\255\255\255\255\027\001\022\001\
\029\001\004\001\255\255\255\255\027\001\255\255\029\001\255\255\
\255\255\012\001\013\001\255\255\255\255\255\255\017\001\255\255\
\255\255\255\255\255\255\022\001\255\255\255\255\255\255\255\255\
\027\001\255\255\029\001"

let yynames_const = "\
  ADD\000\
  SEMICOLON\000\
  COLON\000\
  GTEQ\000\
  EQUAL\000\
  ASSIGN\000\
  DEREF\000\
  ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  WHILE\000\
  DO\000\
  SKIP\000\
  BOOL\000\
  INTTYPE\000\
  LET\000\
  IN\000\
  FN\000\
  VAL\000\
  REC\000\
  LPAREN\000\
  RPAREN\000\
  BEGIN\000\
  END\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 31 "parser.mly"
                         ( _1 )
# 268 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 34 "parser.mly"
                                     ( Past.Integer (get_loc(), _1) )
# 275 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
                                     ( Past.Var (get_loc(), _1) )
# 282 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                                     ( Past.Boolean (get_loc(), true))
# 288 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                                     ( Past.Boolean (get_loc(), false))
# 294 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 38 "parser.mly"
                                     ( _2 )
# 301 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 39 "parser.mly"
                                   ( Past.Deref(get_loc(), _2) )
# 308 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 40 "parser.mly"
                                  ( Past.Ref(get_loc(), _2) )
# 315 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 43 "parser.mly"
                                     (  _1 )
# 322 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 44 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.ADD, _3) )
# 330 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 45 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.GTEQ, _3) )
# 338 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 46 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.EQ, _3) )
# 346 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 47 "parser.mly"
                                     ( Past.Assign(get_loc(), _1, _3) )
# 354 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.expr list) in
    Obj.repr(
# 48 "parser.mly"
                                     ( Past.Seq(get_loc(), _2) )
# 361 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Past.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 49 "parser.mly"
                                     ( Past.If(get_loc(), _2, _4, _6) )
# 370 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 50 "parser.mly"
                                     ( Past.While(get_loc(), _2, _4) )
# 378 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.expr list) in
    Obj.repr(
# 51 "parser.mly"
                                     ( Past.Seq(get_loc(), _2) )
# 385 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'texpr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 53 "parser.mly"
                                     ( Past.Lambda(get_loc(), (_3, _5, _8)) )
# 394 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'texpr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : Past.expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 54 "parser.mly"
                                                         ( Past.Let (get_loc(), _2, _4, _6, _8) )
# 404 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 12 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 8 : 'texpr) in
    let _9 = (Parsing.peek_val __caml_parser_env 5 : 'texpr) in
    let _11 = (Parsing.peek_val __caml_parser_env 3 : Past.expr) in
    let _13 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 56 "parser.mly"
                                     ( Past.LetFun (get_loc(), _2, (_4, _6, _11), _9, _13) )
# 416 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 59 "parser.mly"
                                     ( [_1] )
# 423 "parser.ml"
               : Past.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr list) in
    Obj.repr(
# 60 "parser.mly"
                                     ( _1 :: _3  )
# 431 "parser.ml"
               : Past.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                     ( Past.TEbool  )
# 437 "parser.ml"
               : 'texpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                                     ( Past.TEint  )
# 443 "parser.ml"
               : 'texpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'texpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'texpr) in
    Obj.repr(
# 65 "parser.mly"
                                     ( Past.TEarrow (_1, _3))
# 451 "parser.ml"
               : 'texpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'texpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'texpr) in
    Obj.repr(
# 66 "parser.mly"
                                     ( Past.TEunion (_1, _3))
# 459 "parser.ml"
               : 'texpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'texpr) in
    Obj.repr(
# 67 "parser.mly"
                                     ( _2 )
# 466 "parser.ml"
               : 'texpr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Past.expr)
