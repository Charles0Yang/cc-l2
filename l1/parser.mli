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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Past.expr
