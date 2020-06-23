(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
       | VBool of bool
       | VDClosure of (t -> t)
       | VClosure of (t -> (t -> t) -> t)
       | VCont of (t -> t)
       | VError of string

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VDClosure (t) -> "<fun"
  | VClosure (t) -> "<fun>"
  | VCont (t) -> "<continuation>"
  | VError (s) -> "Error: " ^ s

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
