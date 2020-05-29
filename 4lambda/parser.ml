type token =
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQUAL
  | NOTEQUAL
  | LESS
  | LESSEQUAL
  | GREATER
  | GREATEREQUAL
  | NUMBER of (int)
  | VAR of (string)
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | LET
  | REC
  | IN
  | FUN
  | ARROW
  | TRY
  | WITH
  | SHIFT
  | RESET
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* 補助的な変数、関数、型などの定義 *)
let make_fun vars expr =
  List.fold_right (fun v e -> Syntax.Fun (v, e)) vars expr
# 39 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* DIVIDE *);
  263 (* EQUAL *);
  264 (* NOTEQUAL *);
  265 (* LESS *);
  266 (* LESSEQUAL *);
  267 (* GREATER *);
  268 (* GREATEREQUAL *);
  271 (* TRUE *);
  272 (* FALSE *);
  273 (* IF *);
  274 (* THEN *);
  275 (* ELSE *);
  276 (* LET *);
  277 (* REC *);
  278 (* IN *);
  279 (* FUN *);
  280 (* ARROW *);
  281 (* TRY *);
  282 (* WITH *);
  283 (* SHIFT *);
  284 (* RESET *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  269 (* NUMBER *);
  270 (* VAR *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\004\000\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\003\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\006\000\007\000\009\000\005\000\004\000\004\000\003\000\
\001\000\000\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\001\000\004\000\002\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\000\029\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yydgoto = "\002\000\
\015\000\016\000\042\000\017\000"

let yysindex = "\012\000\
\095\255\000\000\095\255\095\255\000\000\000\000\000\000\000\000\
\095\255\000\255\003\255\095\255\008\255\007\255\221\255\029\255\
\029\255\211\255\000\000\194\255\009\255\018\255\009\255\255\254\
\011\255\095\255\095\255\095\255\095\255\095\255\095\255\095\255\
\095\255\095\255\095\255\095\255\000\000\000\000\000\000\095\255\
\009\255\017\255\020\255\013\255\095\255\095\255\221\255\010\255\
\010\255\000\000\000\000\068\255\068\255\068\255\068\255\068\255\
\068\255\177\255\000\000\095\255\009\255\095\255\221\255\221\255\
\095\255\137\255\032\255\221\255\221\255\095\255\095\255\221\255\
\157\255\095\255\221\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\001\000\
\026\000\000\000\000\000\000\000\034\255\000\000\007\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\255\000\000\000\000\000\000\000\000\000\000\066\000\047\000\
\068\000\000\000\000\000\079\000\081\000\091\000\093\000\102\000\
\104\000\000\000\000\000\000\000\034\255\000\000\114\000\116\000\
\000\000\000\000\000\000\125\000\127\000\000\000\000\000\137\000\
\000\000\000\000\139\000"

let yygindex = "\000\000\
\024\000\003\000\233\255\000\000"

let yytablesize = 421
let yytable = "\044\000\
\006\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\026\000\001\000\021\000\029\000\030\000\
\023\000\059\000\037\000\038\000\022\000\025\000\041\000\060\000\
\045\000\025\000\018\000\019\000\026\000\003\000\026\000\043\000\
\020\000\061\000\046\000\024\000\062\000\067\000\071\000\030\000\
\026\000\005\000\006\000\007\000\008\000\000\000\007\000\000\000\
\000\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\000\000\000\000\000\000\058\000\
\000\000\024\000\000\000\008\000\063\000\064\000\027\000\028\000\
\029\000\030\000\000\000\000\000\000\000\000\000\011\000\000\000\
\012\000\000\000\000\000\066\000\000\000\068\000\000\000\000\000\
\069\000\000\000\013\000\000\000\014\000\072\000\073\000\003\000\
\000\000\075\000\004\000\000\000\000\000\015\000\000\000\016\000\
\000\000\000\000\000\000\005\000\006\000\007\000\008\000\009\000\
\000\000\022\000\010\000\023\000\000\000\011\000\000\000\012\000\
\000\000\013\000\014\000\000\000\021\000\000\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\000\000\020\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\070\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\074\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\065\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\000\000\000\000\
\000\000\000\000\000\000\040\000\039\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\000\000\000\000\000\000\
\000\000\000\000\006\000\006\000\000\000\000\000\006\000\000\000\
\000\000\000\000\006\000\025\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\000\000\000\000\
\000\000\000\000\000\000\025\000\025\000\000\000\000\000\025\000\
\007\000\007\000\007\000\025\000\000\000\007\000\007\000\007\000\
\007\000\007\000\007\000\000\000\000\000\000\000\000\000\000\000\
\007\000\007\000\000\000\024\000\007\000\008\000\008\000\008\000\
\007\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
\011\000\000\000\012\000\024\000\024\000\008\000\008\000\024\000\
\000\000\008\000\000\000\024\000\013\000\008\000\014\000\000\000\
\011\000\011\000\012\000\012\000\011\000\000\000\012\000\015\000\
\011\000\016\000\012\000\000\000\013\000\013\000\014\000\014\000\
\013\000\000\000\014\000\022\000\013\000\023\000\014\000\015\000\
\015\000\016\000\016\000\015\000\000\000\016\000\021\000\015\000\
\018\000\016\000\000\000\022\000\022\000\023\000\023\000\022\000\
\000\000\023\000\019\000\022\000\020\000\023\000\021\000\021\000\
\018\000\018\000\021\000\000\000\018\000\000\000\021\000\000\000\
\018\000\000\000\019\000\019\000\020\000\020\000\019\000\000\000\
\020\000\000\000\019\000\000\000\020\000"

let yycheck = "\023\000\
\000\000\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\007\001\001\000\014\001\005\001\006\001\
\014\001\041\000\016\000\017\000\021\001\014\001\014\001\007\001\
\026\001\000\000\003\000\004\000\024\001\001\001\024\001\014\001\
\009\000\014\001\024\001\012\000\024\001\061\000\007\001\000\000\
\007\001\013\001\014\001\015\001\016\001\255\255\000\000\255\255\
\255\255\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\255\255\255\255\255\255\040\000\
\255\255\000\000\255\255\000\000\045\000\046\000\003\001\004\001\
\005\001\006\001\255\255\255\255\255\255\255\255\000\000\255\255\
\000\000\255\255\255\255\060\000\255\255\062\000\255\255\255\255\
\065\000\255\255\000\000\255\255\000\000\070\000\071\000\001\001\
\255\255\074\000\004\001\255\255\255\255\000\000\255\255\000\000\
\255\255\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\255\255\000\000\020\001\000\000\255\255\023\001\255\255\025\001\
\255\255\027\001\028\001\255\255\000\000\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\000\000\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\022\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\019\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\255\255\255\255\018\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\255\255\018\001\019\001\255\255\255\255\022\001\255\255\
\255\255\255\255\026\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\255\255\255\255\018\001\019\001\255\255\255\255\022\001\
\002\001\003\001\004\001\026\001\255\255\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\255\255\255\255\255\255\255\255\
\018\001\019\001\255\255\002\001\022\001\002\001\003\001\004\001\
\026\001\255\255\007\001\008\001\009\001\010\001\011\001\012\001\
\002\001\255\255\002\001\018\001\019\001\018\001\019\001\022\001\
\255\255\022\001\255\255\026\001\002\001\026\001\002\001\255\255\
\018\001\019\001\018\001\019\001\022\001\255\255\022\001\002\001\
\026\001\002\001\026\001\255\255\018\001\019\001\018\001\019\001\
\022\001\255\255\022\001\002\001\026\001\002\001\026\001\018\001\
\019\001\018\001\019\001\022\001\255\255\022\001\002\001\026\001\
\002\001\026\001\255\255\018\001\019\001\018\001\019\001\022\001\
\255\255\022\001\002\001\026\001\002\001\026\001\018\001\019\001\
\018\001\019\001\022\001\255\255\022\001\255\255\026\001\255\255\
\026\001\255\255\018\001\019\001\018\001\019\001\022\001\255\255\
\022\001\255\255\026\001\255\255\026\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  EQUAL\000\
  NOTEQUAL\000\
  LESS\000\
  LESSEQUAL\000\
  GREATER\000\
  GREATEREQUAL\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LET\000\
  REC\000\
  IN\000\
  FUN\000\
  ARROW\000\
  TRY\000\
  WITH\000\
  SHIFT\000\
  RESET\000\
  EOF\000\
  "

let yynames_block = "\
  NUMBER\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
        ( Syntax.Number (_1) )
# 282 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
        ( Syntax.Bool (true) )
# 288 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
        ( Syntax.Bool (false) )
# 294 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser.mly"
        ( Syntax.Var (_1) )
# 301 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Syntax.t) in
    Obj.repr(
# 50 "parser.mly"
        ( _2 )
# 308 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 54 "parser.mly"
        ( _1 )
# 315 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 56 "parser.mly"
        ( Syntax.Op (_1, Syntax.Plus, _3) )
# 323 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 58 "parser.mly"
        ( Syntax.Op (_1, Syntax.Minus, _3) )
# 331 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 60 "parser.mly"
        ( Syntax.Op (_1, Syntax.Times, _3) )
# 339 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 62 "parser.mly"
        ( Syntax.Op (_1, Syntax.Divide, _3) )
# 347 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 64 "parser.mly"
        ( Syntax.Op (_1, Syntax.Equal, _3) )
# 355 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 66 "parser.mly"
        ( Syntax.Op (_1, Syntax.NotEqual, _3) )
# 363 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 68 "parser.mly"
        ( Syntax.Op (_1, Syntax.Less, _3) )
# 371 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 70 "parser.mly"
        ( Syntax.Op (_1, Syntax.LessEqual, _3) )
# 379 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 72 "parser.mly"
        ( Syntax.Op (_3, Syntax.Less, _1) )
# 387 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 74 "parser.mly"
        ( Syntax.Op (_3, Syntax.LessEqual, _1) )
# 395 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 76 "parser.mly"
        ( Syntax.Op (Syntax.Number (0), Syntax.Minus, _2) )
# 402 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 78 "parser.mly"
        ( Syntax.If (_2, _4, _6) )
# 411 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 80 "parser.mly"
        ( Syntax.Let (_2, make_fun _3 _5, _7) )
# 421 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'vars) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 82 "parser.mly"
        ( Syntax.Letrec (_3, _4, make_fun _5 _7, _9) )
# 432 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 84 "parser.mly"
        ( Syntax.Fun (_2, make_fun _3 _5) )
# 441 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 86 "parser.mly"
        ( Syntax.Try (_2, _4) )
# 449 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 88 "parser.mly"
        ( Syntax.Shift (_2, _4) )
# 457 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 90 "parser.mly"
        ( Syntax.Reset (_3) )
# 464 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app) in
    Obj.repr(
# 92 "parser.mly"
        ( _1 )
# 471 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
        ( [] )
# 477 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vars) in
    Obj.repr(
# 98 "parser.mly"
        ( _1 :: _2 )
# 485 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 102 "parser.mly"
        ( Syntax.App (_1, _2) )
# 493 "parser.ml"
               : 'app))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 104 "parser.mly"
        ( Syntax.App (_1, _2) )
# 501 "parser.ml"
               : 'app))
(* Entry expr *)
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
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.t)
