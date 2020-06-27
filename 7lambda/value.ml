(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
       | VBool of bool
       | VDClosure of (t -> t)
       | VClosure of (t -> (t -> t) -> t)
       | VCont of (t -> (t -> t) -> t)
       | VError of string
       | HV of t
       | H of (t -> (t -> t) -> t) * ((t -> (t -> t) -> t) -> (t -> t) -> t)
       (* | Vhs_stop of (t -> (t -> t) -> t)
       | Vhr_stop of (t -> (t -> t) -> t) *)

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VDClosure (t) -> "<fun"
  | VClosure (t) -> "<fun>"
  | VCont (t) -> "<continuation>"
  | VError (s) -> "Error: " ^ s
  | HV (v) -> to_string v
  | H (c, f) -> "<contains shift>"
  (* | Vhs_stop (t) -> "<hs-stop>"
  | Vhr_stop (t) -> "<hr-stop>" *)

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
