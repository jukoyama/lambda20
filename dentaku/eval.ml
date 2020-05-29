open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.t *)
let rec f expr k =
  let binop op arg1 arg2 k =
    f arg1 (fun v1 ->f arg2 (fun v2 ->
    begin match (v1, v2) with
    | (VNumber (n1), VNumber (n2)) -> k (VNumber (op n1 n2))
    | (VError (str), _) -> VError (str)
    | (_, VError (str)) -> VError (str)
    end ))
    (* let v1 = f arg1 in
    let v2 = f arg2 in
    begin match (v1, v2) with
    | (VNumber (n1), VNumber (n2)) -> VNumber (op n1 n2)
    | (VError (str), _) -> VError (str)
    | (_, VError (str)) -> VError (str)
       end *)
  in
  match expr with
    Number (n) -> k (VNumber (n))
  | Op (arg1, Plus,   arg2) -> binop ( + ) arg1 arg2 k
  | Op (arg1, Minus,  arg2) -> binop ( - ) arg1 arg2 k
  | Op (arg1, Times,  arg2) -> binop ( * ) arg1 arg2 k
  | Op (arg1, Divide, arg2) ->
    f arg1 (fun v1 -> f arg2 (fun v2 ->
      begin match (v1, v2) with
      | (_, VNumber (0)) -> VError ("Division by zero")
      | (_, _) -> binop ( / ) arg1 arg2 k
      end ))
    (* let v1 = f arg1 in
    let v2 = f arg2 in
    begin match (v1, v2) with
    | (_, VNumber(0)) -> VError("Division by zero")
    | (VNumber(n1), VNumber(n2)) -> VNumber (n1 / n2)
    | (VError (str), _) -> VError (str)
    | (_, VError (str)) -> VError (str)
    end *)
