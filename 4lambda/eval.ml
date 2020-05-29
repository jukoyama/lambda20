open Syntax
open Value

(* DS version *)

(* 実際の計算をする関数 *)
(* Eval.g1 : Syntax.t -> (string, Value.t) Env.t -> Value.t *)
let rec g1 expr env = match expr with
    Number (n) -> VNumber (n)
  | Bool (b) -> VBool (b)
  | Var (x) ->
      begin try
        Env.get env x
      with Not_found -> VError ("Unbound variable: " ^ x) end
  | Op (arg1, op, arg2) ->
      let v1 = g1 arg1 env in
      let v2 = g1 arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) ->
            begin match op with
                Plus      -> VNumber (n1 + n2)
              | Minus     -> VNumber (n1 - n2)
              | Times     -> VNumber (n1 * n2)
              | Divide    -> if n2 = 0 then VError "Division by zero"
                             else VNumber (n1 / n2)
              | Equal     -> VBool (n1 = n2)
              | NotEqual  -> VBool (n1 <> n2)
              | Less      -> VBool (n1 < n2)
              | LessEqual -> VBool (n1 <= n2)
            end
        | (VError (s), _) -> VError (s)
        | (_, VError (s)) -> VError (s)
        | (_, _) -> VError ("Bad arguments to" ^ op_to_string op ^ ": " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
      end
  | If (p, t, e) ->
      let v = g1 p env in
      begin match v with
          VBool (true) -> g1 t env
        | VBool (false) -> g1 e env
        | VError (s) -> VError (s)
        | _ -> VError ("Bad predicate for if: " ^
                       Value.to_string v)
      end
  | Let (x, t1, t2) ->
      let v1 = g1 t1 env in
      let new_env = Env.extend env x v1 in
      g1 t2 new_env
  | Letrec (f, x, t1, t2) ->
      let rec g = VDClosure (fun v -> g1 t1 (Env.extend (Env.extend env x v) f g)) in
      let new_env = Env.extend env f g in
      (* let v1 = VDClosure (fun v -> g1 t1 (Env.extend (Env.extend env x v) f (VDClosure (fun v' -> v')))) in
      let new_env = Env.extend env f v1 in *)
      g1 t2 new_env
  | Fun (x, t) ->
      VDClosure (fun v -> g1 t (Env.extend env x v))
  | App (t1, t2) ->
      let v1 = g1 t1 env in
      let v2 = g1 t2 env in
      begin match v1 with
          VDClosure (f) -> f v2
        | VError (s) -> VError (s)
        | _ -> VError ("Not a function: " ^
                       Value.to_string v1)
      end
  | Try (t1, t2) ->
      let v1 = g1 t1 env in
      begin match v1 with
          VError (n)  -> g1 t2 env
        | _ -> v1
      end
  | Shift (x, t) -> VError "Shift not supported yet."
  | Reset (t) -> VError "Reset not supported yet."

(* CPS style *)
(* Eval.g2 : Syntax.t -> (string, Value.t) Env.t ->  ? -> Value.t *)
let rec g2 expr env k = match expr with
    Number (n) -> k (VNumber (n))
  | Bool (b)   -> k (VBool (b))
  | Var (x) ->
      begin try
        k (Env.get env x)
      with Not_found -> VError ("Unbound variable: " ^ x) end
  | Op (arg1, op, arg2) ->
    g2 arg1 env (fun v1 -> g2 arg2 env (fun v2 ->
      begin match (v1, v2) with
        (VNumber (n1), VNumber (n2)) ->
          begin match op with
              Plus      -> k (VNumber (n1 + n2))
            | Minus     -> k (VNumber (n1 - n2))
            | Times     -> k (VNumber (n1 * n2))
            | Divide    -> if n2 = 0 then k (VError "Division by zero")
                           else k (VNumber (n1 / n2))
            | Equal     -> k (VBool (n1 = n2))
            | NotEqual  -> k (VBool (n1 <> n2))
            | Less      -> k (VBool (n1 < n2))
            | LessEqual -> k (VBool (n1 <= n2))
          end
      | (VError (s), _) -> k (VError (s))
      | (_, VError (s)) -> k (VError (s))
      | (_, _) -> VError ("Bad arguments to" ^ op_to_string op ^ ": " ^
                          Value.to_string v1 ^ ", " ^
                          Value.to_string v2)
      end))
  | If (p, t, e) ->
      g2 p env (fun v ->
        begin match v with
            VBool (true) -> g2 t env k
          | VBool (false) -> g2 e env k
          | VError (s) -> VError (s)
          | _ -> VError ("Bad predicate for if: " ^
                         Value.to_string v)
        end)
  | Let (x, t1, t2) ->
      g2 t1 env (fun v1 ->
        let new_env = Env.extend env x v1 in
        g2 t2 new_env k)
  | Letrec (f, x, t1, t2) ->
      let rec g = VClosure (fun v k -> g2 t1 (Env.extend (Env.extend env x v) f g) k) in
      let new_env = Env.extend env f g in
      g2 t2 new_env k
  | Fun (x, t) ->
      k (VClosure (fun v k' -> g2 t (Env.extend env x v) k'))
  | App (t1, t2) ->
      g2 t1 env (fun v1 -> g2 t2 env (fun v2 ->
        begin match v1 with
            VClosure (f) -> f v2 k
          | VError (s) -> VError (s)
          | _ -> VError ("Not a function: " ^
                         Value.to_string v1)
        end
      ))
  | Try (t1, t2) ->
    g2 t1 env (fun v1 ->
      begin match v1 with
          VError (s) -> g2 t2 env k
        | _ -> k (v1)
      end)
  | Shift (x, t) -> VError "Shift not supported yet."
  | Reset (t) -> VError "Reset not supported yet."

(* Eval.f : Syntax.t -> (string, Value.t) Env.t -> Value.t *)
(* let f expr env = g1 expr env *)
let f expr env = g2 expr env (fun x -> x)
