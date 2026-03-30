(* File: eval.ml *)

open Parse_tree_type

exception Exit;;
exception Runtime_Error of string;;

type dict_key =
  | Int_Key of int
  | Float_Key of float
  | Bool_Key of bool
  | String_Key of string
  | Tuple_Key of dict_key list
;;

type value =
  | Int_Val of int
  | Float_Val of float
  | Bool_Val of bool
  | String_Val of string
  | List_Val of value list
  | Tuple_Val of value list
  | Dict_Val of (dict_key, value) Hashtbl.t
;;

type env = (string, value) Hashtbl.t
;;

let mk_env () = Hashtbl.create 20
;;

let lookup env name =
  if Hashtbl.mem env name then
    Hashtbl.find env name
  else
    raise (Runtime_Error ("undefined variable: " ^ name))
;;

let update env name value =
  Hashtbl.replace env name value
;;

let quoted_string s =
  "\"" ^ String.escaped s ^ "\""
;;

let float_string x =
  let s = string_of_float x in
  if String.get s (String.length s - 1) = '.'
  then
    s ^ "0"
  else
    s
;;

let rec string_of_dict_key k =
  match k with
  | Int_Key n ->
      string_of_int n
  | Float_Key x ->
      float_string x
  | Bool_Key true ->
      "True"
  | Bool_Key false ->
      "False"
  | String_Key s ->
      quoted_string s
  | Tuple_Key [] ->
      "()"
  | Tuple_Key [k1] ->
      "(" ^ string_of_dict_key k1 ^ ",)"
  | Tuple_Key ks ->
      "(" ^ string_of_dict_keys ks ^ ")"

and
  string_of_dict_keys ks =
    match ks with
    | [] ->
        ""
    | [k1] ->
        string_of_dict_key k1
    | k1::rest ->
        string_of_dict_key k1 ^ ", " ^ string_of_dict_keys rest
;;

let rec string_of_value v =
  match v with
  | Int_Val n ->
      string_of_int n
  | Float_Val x ->
      float_string x
  | Bool_Val true ->
      "True"
  | Bool_Val false ->
      "False"
  | String_Val s ->
      quoted_string s
  | List_Val vs ->
      "[" ^ string_of_values vs ^ "]"
  | Tuple_Val [] ->
      "()"
  | Tuple_Val [v1] ->
      "(" ^ string_of_value v1 ^ ",)"
  | Tuple_Val vs ->
      "(" ^ string_of_values vs ^ ")"
  | Dict_Val d ->
      "{" ^ string_of_dict_entries d ^ "}"

and
  string_of_values vs =
    match vs with
    | [] ->
        ""
    | [v1] ->
        string_of_value v1
    | v1::rest ->
        string_of_value v1 ^ ", " ^ string_of_values rest

and
  string_of_dict_entries d =
    let entries =
      Hashtbl.fold
        (fun k v acc ->
           (string_of_dict_key k ^ ": " ^ string_of_value v)::acc)
        d
        []
    in
    String.concat ", " (List.sort String.compare entries)
;;

let int_of_bool b =
  if b then 1 else 0
;;

let float_of_bool b =
  if b then 1.0 else 0.0
;;

let rec dict_key_of_value v =
  match v with
  | Int_Val n ->
      Int_Key n
  | Float_Val x ->
      Float_Key x
  | Bool_Val b ->
      Bool_Key b
  | String_Val s ->
      String_Key s
  | Tuple_Val vs ->
      Tuple_Key (List.map dict_key_of_value vs)
  | List_Val _ ->
      raise (Runtime_Error "unhashable type: list")
  | Dict_Val _ ->
      raise (Runtime_Error "unhashable type: dict")
;;

let to_int_value v =
  match v with
  | Int_Val n ->
      n
  | Bool_Val b ->
      int_of_bool b
  | _ ->
      raise (Runtime_Error ("expected int-like value but got " ^ string_of_value v))
;;

let to_float_value v =
  match v with
  | Int_Val n ->
      float_of_int n
  | Float_Val x ->
      x
  | Bool_Val b ->
      float_of_bool b
  | _ ->
      raise (Runtime_Error ("expected numeric value but got " ^ string_of_value v))
;;

let to_bool_value v =
  match v with
  | Int_Val n ->
      n <> 0
  | Float_Val x ->
      x <> 0.0
  | Bool_Val b ->
      b
  | String_Val s ->
      s <> ""
  | List_Val vs ->
      vs <> []
  | Tuple_Val vs ->
      vs <> []
  | Dict_Val d ->
      Hashtbl.length d <> 0
;;

let normalize_index i len =
  let j = if i < 0 then len + i else i in
  if j < 0 || j >= len then
    raise (Runtime_Error "index out of range")
  else
    j
;;

let value_at_index v index_v =
  match v with
  | List_Val xs ->
      let i = normalize_index (to_int_value index_v) (List.length xs) in
      List.nth xs i
  | Tuple_Val xs ->
      let i = normalize_index (to_int_value index_v) (List.length xs) in
      List.nth xs i
  | Dict_Val d ->
      let k = dict_key_of_value index_v in
      if Hashtbl.mem d k then
        Hashtbl.find d k
      else
        raise (Runtime_Error ("key not found: " ^ string_of_value index_v))
  | Int_Val _ ->
      raise (Runtime_Error "type int is not indexable")
  | Float_Val _ ->
      raise (Runtime_Error "type float is not indexable")
  | Bool_Val _ ->
      raise (Runtime_Error "type bool is not indexable")
  | String_Val _ ->
      raise (Runtime_Error "type string is not indexable")
;;

let replace_nth xs i new_v =
  let rec aux j ys =
    match ys with
    | [] ->
        raise (Runtime_Error "index out of range")
    | _::rest when j = 0 ->
        new_v::rest
    | y::rest ->
        y::(aux (j - 1) rest)
  in
  aux i xs
;;

let target_root_and_indices target =
  let rec aux t acc =
    match t with
    | Name_Target name ->
        (name, List.rev acc)
    | Index_Target (t1, idx_e) ->
        aux t1 (idx_e::acc)
  in
  aux target []
;;

let neg_value v =
  match v with
  | Int_Val n ->
      Int_Val (-n)
  | Float_Val x ->
      Float_Val (-.x)
  | Bool_Val b ->
      Int_Val (-(int_of_bool b))
  | String_Val _ ->
      raise (Runtime_Error "bad operand type for unary -: string")
  | List_Val _ ->
      raise (Runtime_Error "bad operand type for unary -: list")
  | Tuple_Val _ ->
      raise (Runtime_Error "bad operand type for unary -: tuple")
  | Dict_Val _ ->
      raise (Runtime_Error "bad operand type for unary -: dict")
;;

let add_values v1 v2 =
  match (v1, v2) with
  | (String_Val a, String_Val b) ->
      String_Val (a ^ b)
  | (List_Val xs, List_Val ys) ->
      List_Val (xs @ ys)
  | (Tuple_Val xs, Tuple_Val ys) ->
      Tuple_Val (xs @ ys)
  | (Dict_Val _, _) | (_, Dict_Val _) ->
      raise (Runtime_Error "unsupported operand type for + with dict")
  | (String_Val _, _) | (_, String_Val _) ->
      raise (Runtime_Error "cannot add string to non-string")
  | (List_Val _, _) | (_, List_Val _) ->
      raise (Runtime_Error "can only concatenate list to list")
  | (Tuple_Val _, _) | (_, Tuple_Val _) ->
      raise (Runtime_Error "can only concatenate tuple to tuple")
  | (Float_Val a, Float_Val b) ->
      Float_Val (a +. b)
  | (Float_Val a, _) ->
      Float_Val (a +. to_float_value v2)
  | (_, Float_Val b) ->
      Float_Val (to_float_value v1 +. b)
  | _ ->
      Int_Val (to_int_value v1 + to_int_value v2)
;;

let sub_values v1 v2 =
  match (v1, v2) with
  | (String_Val _, _) | (_, String_Val _) ->
      raise (Runtime_Error "unsupported operand type for - with string")
  | (List_Val _, _) | (_, List_Val _) ->
      raise (Runtime_Error "unsupported operand type for - with list")
  | (Tuple_Val _, _) | (_, Tuple_Val _) ->
      raise (Runtime_Error "unsupported operand type for - with tuple")
  | (Dict_Val _, _) | (_, Dict_Val _) ->
      raise (Runtime_Error "unsupported operand type for - with dict")
  | (Float_Val a, Float_Val b) ->
      Float_Val (a -. b)
  | (Float_Val a, _) ->
      Float_Val (a -. to_float_value v2)
  | (_, Float_Val b) ->
      Float_Val (to_float_value v1 -. b)
  | _ ->
      Int_Val (to_int_value v1 - to_int_value v2)
;;

let mul_values v1 v2 =
  match (v1, v2) with
  | (String_Val _, _) | (_, String_Val _) ->
      raise (Runtime_Error "unsupported operand type for * with string")
  | (List_Val _, _) | (_, List_Val _) ->
      raise (Runtime_Error "unsupported operand type for * with list")
  | (Tuple_Val _, _) | (_, Tuple_Val _) ->
      raise (Runtime_Error "unsupported operand type for * with tuple")
  | (Dict_Val _, _) | (_, Dict_Val _) ->
      raise (Runtime_Error "unsupported operand type for * with dict")
  | (Float_Val a, Float_Val b) ->
      Float_Val (a *. b)
  | (Float_Val a, _) ->
      Float_Val (a *. to_float_value v2)
  | (_, Float_Val b) ->
      Float_Val (to_float_value v1 *. b)
  | _ ->
      Int_Val (to_int_value v1 * to_int_value v2)
;;

let div_values v1 v2 =
  match (v1, v2) with
  | (String_Val _, _) | (_, String_Val _) ->
      raise (Runtime_Error "unsupported operand type for / with string")
  | (List_Val _, _) | (_, List_Val _) ->
      raise (Runtime_Error "unsupported operand type for / with list")
  | (Tuple_Val _, _) | (_, Tuple_Val _) ->
      raise (Runtime_Error "unsupported operand type for / with tuple")
  | (Dict_Val _, _) | (_, Dict_Val _) ->
      raise (Runtime_Error "unsupported operand type for / with dict")
  | (Float_Val a, Float_Val b) ->
      if b = 0.0 then
        raise (Runtime_Error "division by zero")
      else
        Float_Val (a /. b)
  | (Float_Val a, _) ->
      let b = to_float_value v2 in
      if b = 0.0 then
        raise (Runtime_Error "division by zero")
      else
        Float_Val (a /. b)
  | (_, Float_Val b) ->
      if b = 0.0 then
        raise (Runtime_Error "division by zero")
      else
        Float_Val (to_float_value v1 /. b)
  | _ ->
      let b = to_int_value v2 in
      if b = 0 then
        raise (Runtime_Error "division by zero")
      else
        Int_Val (to_int_value v1 / b)
;;

let rec int_pow a b =
  if b < 0 then
    raise (Runtime_Error "negative integer exponent")
  else if b = 0 then
    1
  else if b mod 2 = 0 then
    let half = int_pow a (b / 2) in
    half * half
  else
    a * int_pow a (b - 1)
;;

let pow_values v1 v2 =
  match (v1, v2) with
  | (String_Val _, _) | (_, String_Val _) ->
      raise (Runtime_Error "unsupported operand type for ** with string")
  | (List_Val _, _) | (_, List_Val _) ->
      raise (Runtime_Error "unsupported operand type for ** with list")
  | (Tuple_Val _, _) | (_, Tuple_Val _) ->
      raise (Runtime_Error "unsupported operand type for ** with tuple")
  | (Dict_Val _, _) | (_, Dict_Val _) ->
      raise (Runtime_Error "unsupported operand type for ** with dict")
  | (Float_Val a, Float_Val b) ->
      Float_Val (a ** b)
  | (Float_Val a, _) ->
      Float_Val (a ** to_float_value v2)
  | (_, Float_Val b) ->
      Float_Val (to_float_value v1 ** b)
  | _ ->
      let a = to_int_value v1 in
      let b = to_int_value v2 in
      if b < 0 then
        Float_Val ((float_of_int a) ** (float_of_int b))
      else
        Int_Val (int_pow a b)
;;

let rec value_equal v1 v2 =
  match (v1, v2) with
  | (Int_Val a, Int_Val b) ->
      a = b
  | (Bool_Val a, Bool_Val b) ->
      a = b
  | (Int_Val a, Bool_Val b) ->
      a = int_of_bool b
  | (Bool_Val a, Int_Val b) ->
      int_of_bool a = b
  | (Float_Val a, Float_Val b) ->
      a = b
  | (Float_Val a, Int_Val b) ->
      a = float_of_int b
  | (Int_Val a, Float_Val b) ->
      float_of_int a = b
  | (Float_Val a, Bool_Val b) ->
      a = float_of_bool b
  | (Bool_Val a, Float_Val b) ->
      float_of_bool a = b
  | (String_Val a, String_Val b) ->
      a = b
  | (List_Val xs, List_Val ys) ->
      value_list_equal xs ys
  | (Tuple_Val xs, Tuple_Val ys) ->
      value_list_equal xs ys
  | (Dict_Val d1, Dict_Val d2) ->
      dict_equal d1 d2
  | _ ->
      false

and
  value_list_equal xs ys =
    match (xs, ys) with
    | ([], []) ->
        true
    | (x1::xs1, y1::ys1) ->
        value_equal x1 y1 && value_list_equal xs1 ys1
    | _ ->
        false

and
  dict_equal d1 d2 =
    if Hashtbl.length d1 <> Hashtbl.length d2 then
      false
    else
      Hashtbl.fold
        (fun k v acc ->
           acc &&
           Hashtbl.mem d2 k &&
           value_equal v (Hashtbl.find d2 k))
        d1
        true
;;

let eq_values v1 v2 =
  Bool_Val (value_equal v1 v2)
;;

let neq_values v1 v2 =
  match eq_values v1 v2 with
  | Bool_Val b ->
      Bool_Val (not b)
  | _ ->
      raise (Runtime_Error "internal comparison error")
;;

let lt_values v1 v2 =
  match (v1, v2) with
  | (String_Val a, String_Val b) ->
      Bool_Val (a < b)
  | (String_Val _, _) | (_, String_Val _) ->
      raise (Runtime_Error "cannot compare string with non-string")
  | (List_Val _, _) | (_, List_Val _) ->
      raise (Runtime_Error "cannot order-compare lists")
  | (Tuple_Val _, _) | (_, Tuple_Val _) ->
      raise (Runtime_Error "cannot order-compare tuples")
  | (Dict_Val _, _) | (_, Dict_Val _) ->
      raise (Runtime_Error "cannot order-compare dicts")
  | (Float_Val a, Float_Val b) ->
      Bool_Val (a < b)
  | (Float_Val a, _) ->
      Bool_Val (a < to_float_value v2)
  | (_, Float_Val b) ->
      Bool_Val (to_float_value v1 < b)
  | _ ->
      Bool_Val (to_int_value v1 < to_int_value v2)
;;

let leq_values v1 v2 =
  match (v1, v2) with
  | (String_Val a, String_Val b) ->
      Bool_Val (a <= b)
  | (String_Val _, _) | (_, String_Val _) ->
      raise (Runtime_Error "cannot compare string with non-string")
  | (List_Val _, _) | (_, List_Val _) ->
      raise (Runtime_Error "cannot order-compare lists")
  | (Tuple_Val _, _) | (_, Tuple_Val _) ->
      raise (Runtime_Error "cannot order-compare tuples")
  | (Dict_Val _, _) | (_, Dict_Val _) ->
      raise (Runtime_Error "cannot order-compare dicts")
  | (Float_Val a, Float_Val b) ->
      Bool_Val (a <= b)
  | (Float_Val a, _) ->
      Bool_Val (a <= to_float_value v2)
  | (_, Float_Val b) ->
      Bool_Val (to_float_value v1 <= b)
  | _ ->
      Bool_Val (to_int_value v1 <= to_int_value v2)
;;

let gt_values v1 v2 =
  match (v1, v2) with
  | (String_Val a, String_Val b) ->
      Bool_Val (a > b)
  | (String_Val _, _) | (_, String_Val _) ->
      raise (Runtime_Error "cannot compare string with non-string")
  | (List_Val _, _) | (_, List_Val _) ->
      raise (Runtime_Error "cannot order-compare lists")
  | (Tuple_Val _, _) | (_, Tuple_Val _) ->
      raise (Runtime_Error "cannot order-compare tuples")
  | (Dict_Val _, _) | (_, Dict_Val _) ->
      raise (Runtime_Error "cannot order-compare dicts")
  | (Float_Val a, Float_Val b) ->
      Bool_Val (a > b)
  | (Float_Val a, _) ->
      Bool_Val (a > to_float_value v2)
  | (_, Float_Val b) ->
      Bool_Val (to_float_value v1 > b)
  | _ ->
      Bool_Val (to_int_value v1 > to_int_value v2)
;;

let geq_values v1 v2 =
  match (v1, v2) with
  | (String_Val a, String_Val b) ->
      Bool_Val (a >= b)
  | (String_Val _, _) | (_, String_Val _) ->
      raise (Runtime_Error "cannot compare string with non-string")
  | (List_Val _, _) | (_, List_Val _) ->
      raise (Runtime_Error "cannot order-compare lists")
  | (Tuple_Val _, _) | (_, Tuple_Val _) ->
      raise (Runtime_Error "cannot order-compare tuples")
  | (Dict_Val _, _) | (_, Dict_Val _) ->
      raise (Runtime_Error "cannot order-compare dicts")
  | (Float_Val a, Float_Val b) ->
      Bool_Val (a >= b)
  | (Float_Val a, _) ->
      Bool_Val (a >= to_float_value v2)
  | (_, Float_Val b) ->
      Bool_Val (to_float_value v1 >= b)
  | _ ->
      Bool_Val (to_int_value v1 >= to_int_value v2)
;;

let and_values v1 v2 =
  Bool_Val ((to_bool_value v1) && (to_bool_value v2))
;;

let or_values v1 v2 =
  Bool_Val ((to_bool_value v1) || (to_bool_value v2))
;;

let not_value v =
  Bool_Val (not (to_bool_value v))
;;

let rec set_path env current idx_es rhs =
  match idx_es with
  | [] ->
      rhs
  | idx_e::rest ->
      let idx_v = eval_expr env idx_e in
      match current with
      | List_Val xs ->
          let i = normalize_index (to_int_value idx_v) (List.length xs) in
          let child = List.nth xs i in
          let new_child = set_path env child rest rhs in
          List_Val (replace_nth xs i new_child)
      | Tuple_Val _ ->
          raise (Runtime_Error "tuple does not support item assignment")
      | Dict_Val d ->
          let k = dict_key_of_value idx_v in
          (match rest with
           | [] ->
               let _ = Hashtbl.replace d k rhs in
               Dict_Val d
           | _ ->
               if Hashtbl.mem d k then
                 let child = Hashtbl.find d k in
                 let new_child = set_path env child rest rhs in
                 let _ = Hashtbl.replace d k new_child in
                 Dict_Val d
               else
                 raise (Runtime_Error ("key not found: " ^ string_of_value idx_v)))
      | String_Val _ ->
          raise (Runtime_Error "string does not support item assignment")
      | Int_Val _ ->
          raise (Runtime_Error "int does not support item assignment")
      | Float_Val _ ->
          raise (Runtime_Error "float does not support item assignment")
      | Bool_Val _ ->
          raise (Runtime_Error "bool does not support item assignment")

and
  assign_target env target rhs =
    let (root_name, idx_es) = target_root_and_indices target in
    match idx_es with
    | [] ->
        update env root_name rhs
    | _ ->
        let root_v = lookup env root_name in
        let new_root = set_path env root_v idx_es rhs in
        update env root_name new_root

and
  eval_expr env e =
    match e with
    | Int_Expr n ->
        Int_Val n
    | Float_Expr x ->
        Float_Val x
    | Bool_Expr b ->
        Bool_Val b
    | String_Expr s ->
        String_Val s
    | Var_Expr name ->
        lookup env name
    | List_Expr es ->
        List_Val (List.map (eval_expr env) es)
    | Tuple_Expr es ->
        Tuple_Val (List.map (eval_expr env) es)
    | Dict_Expr pairs ->
        let d = Hashtbl.create 20 in
        let rec fill ps =
          match ps with
          | [] ->
              ()
          | (k_expr, v_expr)::rest ->
              let k_val = eval_expr env k_expr in
              let v_val = eval_expr env v_expr in
              let k = dict_key_of_value k_val in
              let _ = Hashtbl.replace d k v_val in
              fill rest
        in
        let _ = fill pairs in
        Dict_Val d
    | Index_Expr (e1, e2) ->
        value_at_index (eval_expr env e1) (eval_expr env e2)
    | Neg_Expr e1 ->
        neg_value (eval_expr env e1)
    | Add_Expr (e1, e2) ->
        add_values (eval_expr env e1) (eval_expr env e2)
    | Sub_Expr (e1, e2) ->
        sub_values (eval_expr env e1) (eval_expr env e2)
    | Pow_Expr (e1, e2) ->
        pow_values (eval_expr env e1) (eval_expr env e2)
    | Mul_Expr (e1, e2) ->
        mul_values (eval_expr env e1) (eval_expr env e2)
    | Div_Expr (e1, e2) ->
        div_values (eval_expr env e1) (eval_expr env e2)
    | Is_Eq_Expr (e1, e2) ->
        eq_values (eval_expr env e1) (eval_expr env e2)
    | Is_Neq_Expr (e1, e2) ->
        neq_values (eval_expr env e1) (eval_expr env e2)
    | Lt_Expr (e1, e2) ->
        lt_values (eval_expr env e1) (eval_expr env e2)
    | Leq_Expr (e1, e2) ->
        leq_values (eval_expr env e1) (eval_expr env e2)
    | Gt_Expr (e1, e2) ->
        gt_values (eval_expr env e1) (eval_expr env e2)
    | Geq_Expr (e1, e2) ->
        geq_values (eval_expr env e1) (eval_expr env e2)
    | And_Expr (e1, e2) ->
        and_values (eval_expr env e1) (eval_expr env e2)
    | Or_Expr (e1, e2) ->
        or_values (eval_expr env e1) (eval_expr env e2)
    | Not_Expr e1 ->
        not_value (eval_expr env e1)
;;

let rec eval_stmt env s =
  match s with
  | Exit_Stmt ->
      raise Exit
  | Expr_Stmt e ->
      let v = eval_expr env e in
      print_endline (string_of_value v)
  | Assign_Stmt (names, e) ->
      let v = eval_expr env e in
      List.iter (fun name -> update env name v) names
  | Target_Assign_Stmt (target, e) ->
      let v = eval_expr env e in
      assign_target env target v
  | If_Stmt (condition, then_body, else_body) ->
      if to_bool_value (eval_expr env condition) then
        eval_stmt_list env then_body
      else
        (match else_body with
         | No_Else ->
             ()
         | Else_Block stmts ->
             eval_stmt_list env stmts)
  | While_Stmt (condition, body) ->
      while to_bool_value (eval_expr env condition) do
        eval_stmt_list env body
      done

and
  eval_stmt_list env stmts =
    List.iter (eval_stmt env) stmts
;;
