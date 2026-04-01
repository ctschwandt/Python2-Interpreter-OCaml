open Value

let int_of_bool b =
  if b then 1 else 0
;;

let float_of_bool b =
  if b then 1.0 else 0.0
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

let require_int_value v =
  match v with
  | Int_Val n ->
      n
  | _ ->
      raise (Runtime_Error ("expected int value but got " ^ string_of_value v))
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

let clamp low high x =
  max low (min high x)
;;

let normalize_slice_bound i len step =
  let j = if i < 0 then len + i else i in
  if step > 0 then
    clamp 0 len j
  else
    clamp (-1) (len - 1) j
;;

let value_at_slice v start_opt stop_opt step_opt =
  let step =
    match step_opt with
    | None ->
        1
    | Some step_v ->
        to_int_value step_v
  in
  if step = 0 then
    raise (Runtime_Error "slice step cannot be zero")
  else
    match v with
    | List_Val xs ->
        let arr = Array.of_list xs in
        let len = Array.length arr in
        let default_start = if step > 0 then 0 else len - 1 in
        let default_stop = if step > 0 then len else -1 in
        let start =
          match start_opt with
          | None ->
              default_start
          | Some start_v ->
              normalize_slice_bound (to_int_value start_v) len step
        in
        let stop =
          match stop_opt with
          | None ->
              default_stop
          | Some stop_v ->
              normalize_slice_bound (to_int_value stop_v) len step
        in
        let rec build i acc =
          if (step > 0 && i >= stop) || (step < 0 && i <= stop) then
            List.rev acc
          else
            build (i + step) (arr.(i)::acc)
        in
        List_Val (build start [])
    | Tuple_Val _ ->
        raise (Runtime_Error "type tuple does not support slicing")
    | Dict_Val _ ->
        raise (Runtime_Error "type dict does not support slicing")
    | Int_Val _ ->
        raise (Runtime_Error "type int does not support slicing")
    | Float_Val _ ->
        raise (Runtime_Error "type float does not support slicing")
    | Bool_Val _ ->
        raise (Runtime_Error "type bool does not support slicing")
    | String_Val _ ->
        raise (Runtime_Error "type string does not support slicing")
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

let bitnot_value v =
  Int_Val (lnot (require_int_value v))
;;

let bitand_values v1 v2 =
  Int_Val ((require_int_value v1) land (require_int_value v2))
;;

let bitor_values v1 v2 =
  Int_Val ((require_int_value v1) lor (require_int_value v2))
;;

let bitxor_values v1 v2 =
  Int_Val ((require_int_value v1) lxor (require_int_value v2))
;;

let lshift_values v1 v2 =
  Int_Val ((require_int_value v1) lsl (require_int_value v2))
;;

let rshift_values v1 v2 =
  Int_Val ((require_int_value v1) asr (require_int_value v2))
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
