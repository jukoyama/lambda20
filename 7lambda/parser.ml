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
  | CONTROL
  | PROMPT
  | SHIFT0
  | RESET0
  | CONTROL0
  | PROMPT0
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* 補助的な変数、関数、型などの定義 *)
let make_fun vars expr =
  List.fold_right (fun v e -> Syntax.Fun (v, e)) vars expr
# 45 "parser.ml"
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
  285 (* CONTROL *);
  286 (* PROMPT *);
  287 (* SHIFT0 *);
  288 (* RESET0 *);
  289 (* CONTROL0 *);
  290 (* PROMPT0 *);
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
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\004\000\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\003\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\006\000\007\000\009\000\005\000\004\000\004\000\002\000\
\004\000\002\000\004\000\002\000\004\000\002\000\001\000\000\000\
\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\001\000\004\000\002\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\000\000\026\000\000\000\030\000\000\000\028\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\000\035\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
\010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\021\000\022\000\054\000\023\000"

let yysindex = "\003\000\
\129\255\000\000\129\255\129\255\000\000\000\000\000\000\000\000\
\129\255\004\255\244\254\129\255\008\255\061\255\009\255\061\255\
\029\255\061\255\030\255\061\255\225\255\061\255\061\255\215\255\
\000\000\198\255\031\255\032\255\031\255\002\255\037\255\000\000\
\039\255\000\000\041\255\000\000\043\255\000\000\129\255\129\255\
\129\255\129\255\129\255\129\255\129\255\129\255\129\255\129\255\
\000\000\000\000\000\000\129\255\031\255\071\255\066\255\058\255\
\129\255\129\255\129\255\129\255\129\255\014\255\014\255\000\000\
\000\000\036\255\036\255\036\255\036\255\036\255\036\255\181\255\
\000\000\129\255\031\255\129\255\225\255\225\255\225\255\225\255\
\225\255\129\255\026\255\076\255\225\255\225\255\129\255\129\255\
\225\255\161\255\129\255\225\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\050\000\001\000\026\000\000\000\
\000\000\000\000\077\255\000\000\063\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\249\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\068\000\000\000\
\000\000\066\000\079\000\081\000\091\000\093\000\102\000\000\000\
\000\000\000\000\077\255\000\000\104\000\114\000\116\000\125\000\
\127\000\000\000\000\000\000\000\137\000\139\000\000\000\000\000\
\148\000\000\000\000\000\150\000"

let yygindex = "\000\000\
\012\000\095\000\230\255\000\000"

let yytablesize = 432
let yytable = "\032\000\
\006\000\029\000\056\000\001\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\024\000\025\000\
\032\000\027\000\041\000\042\000\026\000\031\000\033\000\030\000\
\028\000\031\000\073\000\057\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\039\000\040\000\
\041\000\042\000\035\000\037\000\053\000\055\000\007\000\087\000\
\084\000\036\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\058\000\003\000\059\000\072\000\
\060\000\011\000\061\000\008\000\077\000\078\000\079\000\080\000\
\081\000\005\000\006\000\007\000\008\000\074\000\012\000\075\000\
\013\000\076\000\088\000\032\000\000\000\083\000\032\000\085\000\
\000\000\000\000\014\000\000\000\015\000\086\000\000\000\000\000\
\000\000\000\000\089\000\090\000\000\000\016\000\092\000\022\000\
\000\000\000\000\000\000\000\000\032\000\000\000\034\000\000\000\
\036\000\023\000\038\000\025\000\049\000\050\000\000\000\000\000\
\000\000\000\000\000\000\000\000\029\000\000\000\027\000\000\000\
\000\000\003\000\000\000\000\000\004\000\000\000\000\000\000\000\
\021\000\000\000\018\000\000\000\000\000\005\000\006\000\007\000\
\008\000\009\000\000\000\019\000\010\000\020\000\000\000\011\000\
\000\000\012\000\000\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\091\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\046\000\047\000\
\048\000\000\000\000\000\000\000\000\000\000\000\000\000\082\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\000\000\000\000\000\000\000\000\000\000\052\000\
\051\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\000\000\000\000\000\000\
\000\000\000\000\006\000\006\000\000\000\000\000\006\000\000\000\
\000\000\000\000\006\000\031\000\031\000\031\000\031\000\031\000\
\031\000\031\000\031\000\031\000\031\000\031\000\000\000\000\000\
\000\000\000\000\000\000\031\000\031\000\000\000\000\000\031\000\
\007\000\007\000\007\000\031\000\000\000\007\000\007\000\007\000\
\007\000\007\000\007\000\000\000\000\000\000\000\000\000\000\000\
\007\000\007\000\000\000\011\000\007\000\008\000\008\000\008\000\
\007\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
\012\000\000\000\013\000\011\000\011\000\008\000\008\000\011\000\
\000\000\008\000\000\000\011\000\014\000\008\000\015\000\000\000\
\012\000\012\000\013\000\013\000\012\000\000\000\013\000\016\000\
\012\000\022\000\013\000\000\000\014\000\014\000\015\000\015\000\
\014\000\000\000\015\000\023\000\014\000\025\000\015\000\016\000\
\016\000\022\000\022\000\016\000\000\000\022\000\029\000\016\000\
\027\000\022\000\000\000\023\000\023\000\025\000\025\000\023\000\
\000\000\025\000\021\000\023\000\018\000\025\000\029\000\029\000\
\027\000\027\000\029\000\000\000\027\000\019\000\029\000\020\000\
\027\000\000\000\021\000\021\000\018\000\018\000\021\000\000\000\
\018\000\000\000\021\000\000\000\018\000\019\000\019\000\020\000\
\020\000\019\000\000\000\020\000\000\000\019\000\000\000\020\000"

let yycheck = "\007\001\
\000\000\014\001\029\000\001\000\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\003\000\004\000\
\024\001\014\001\005\001\006\001\009\000\014\001\014\001\012\000\
\021\001\000\000\053\000\026\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\003\001\004\001\
\005\001\006\001\014\001\014\001\014\001\014\001\000\000\022\001\
\075\000\000\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\047\000\048\000\024\001\001\001\024\001\052\000\
\024\001\000\000\024\001\000\000\057\000\058\000\059\000\060\000\
\061\000\013\001\014\001\015\001\016\001\007\001\000\000\014\001\
\000\000\024\001\007\001\007\001\255\255\074\000\024\001\076\000\
\255\255\255\255\000\000\255\255\000\000\082\000\255\255\255\255\
\255\255\255\255\087\000\088\000\255\255\000\000\091\000\000\000\
\255\255\255\255\255\255\255\255\014\000\255\255\016\000\255\255\
\018\000\000\000\020\000\000\000\022\000\023\000\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\000\000\255\255\
\255\255\001\001\255\255\255\255\004\001\255\255\255\255\255\255\
\000\000\255\255\000\000\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\255\255\000\000\020\001\000\000\255\255\023\001\
\255\255\025\001\255\255\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\019\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\255\255\255\255\018\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
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
\018\001\019\001\022\001\255\255\022\001\002\001\026\001\002\001\
\026\001\255\255\018\001\019\001\018\001\019\001\022\001\255\255\
\022\001\255\255\026\001\255\255\026\001\018\001\019\001\018\001\
\019\001\022\001\255\255\022\001\255\255\026\001\255\255\026\001"

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
  CONTROL\000\
  PROMPT\000\
  SHIFT0\000\
  RESET0\000\
  CONTROL0\000\
  PROMPT0\000\
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
# 43 "parser.mly"
        ( Syntax.Number (_1) )
# 310 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
        ( Syntax.Bool (true) )
# 316 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
        ( Syntax.Bool (false) )
# 322 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
        ( Syntax.Var (_1) )
# 329 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Syntax.t) in
    Obj.repr(
# 51 "parser.mly"
        ( _2 )
# 336 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 55 "parser.mly"
        ( _1 )
# 343 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 57 "parser.mly"
        ( Syntax.Op (_1, Syntax.Plus, _3) )
# 351 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 59 "parser.mly"
        ( Syntax.Op (_1, Syntax.Minus, _3) )
# 359 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 61 "parser.mly"
        ( Syntax.Op (_1, Syntax.Times, _3) )
# 367 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 63 "parser.mly"
        ( Syntax.Op (_1, Syntax.Divide, _3) )
# 375 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 65 "parser.mly"
        ( Syntax.Op (_1, Syntax.Equal, _3) )
# 383 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 67 "parser.mly"
        ( Syntax.Op (_1, Syntax.NotEqual, _3) )
# 391 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 69 "parser.mly"
        ( Syntax.Op (_1, Syntax.Less, _3) )
# 399 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 71 "parser.mly"
        ( Syntax.Op (_1, Syntax.LessEqual, _3) )
# 407 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 73 "parser.mly"
        ( Syntax.Op (_3, Syntax.Less, _1) )
# 415 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 75 "parser.mly"
        ( Syntax.Op (_3, Syntax.LessEqual, _1) )
# 423 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 77 "parser.mly"
        ( Syntax.Op (Syntax.Number (0), Syntax.Minus, _2) )
# 430 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 79 "parser.mly"
        ( Syntax.If (_2, _4, _6) )
# 439 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 81 "parser.mly"
        ( Syntax.Let (_2, make_fun _3 _5, _7) )
# 449 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'vars) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 83 "parser.mly"
        ( Syntax.Letrec (_3, _4, make_fun _5 _7, _9) )
# 460 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 85 "parser.mly"
        ( Syntax.Fun (_2, make_fun _3 _5) )
# 469 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 87 "parser.mly"
        ( Syntax.Try (_2, _4) )
# 477 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 89 "parser.mly"
        ( Syntax.Shift (_2, _4) )
# 485 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 91 "parser.mly"
        ( Syntax.Reset (_2) )
# 492 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 93 "parser.mly"
        ( Syntax.Control (_2, _4) )
# 500 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 95 "parser.mly"
        ( Syntax.Prompt (_2) )
# 507 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 97 "parser.mly"
        ( Syntax.Control0 (_2, _4) )
# 515 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 99 "parser.mly"
        ( Syntax.Prompt0 (_2) )
# 522 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 101 "parser.mly"
        ( Syntax.Shift0 (_2, _4) )
# 530 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 103 "parser.mly"
        ( Syntax.Reset0 (_2) )
# 537 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app) in
    Obj.repr(
# 105 "parser.mly"
        ( _1 )
# 544 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
        ( [] )
# 550 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vars) in
    Obj.repr(
# 111 "parser.mly"
        ( _1 :: _2 )
# 558 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 115 "parser.mly"
        ( Syntax.App (_1, _2) )
# 566 "parser.ml"
               : 'app))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 117 "parser.mly"
        ( Syntax.App (_1, _2) )
# 574 "parser.ml"
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
