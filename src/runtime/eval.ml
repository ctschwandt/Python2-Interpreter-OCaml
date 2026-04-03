open Ast
open Value
open Env
open Ops

exception Exit
exception Runtime_Error = Value.Runtime_Error
exception Return_Signal of value

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

let rec set_path env current idx_es rhs =
  match idx_es with
  | [] -> rhs
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
      | Function_Val _ ->
          raise (Runtime_Error "function does not support item assignment")
      | None_Val ->
          raise (Runtime_Error "NoneType does not support item assignment")

and assign_target env target rhs =
  let (root_name, idx_es) = target_root_and_indices target in
  match idx_es with
  | [] -> update env root_name rhs
  | _ ->
      let root_v = lookup env root_name in
      let new_root = set_path env root_v idx_es rhs in
      update env root_name new_root

and eval_expr env e =
  match e with
  | Int_Expr n -> Int_Val n
  | Float_Expr x -> Float_Val x
  | Bool_Expr b -> Bool_Val b
  | String_Expr s -> String_Val s
  | Var_Expr name -> lookup env name
  | List_Expr es -> List_Val (List.map (eval_expr env) es)
  | Tuple_Expr es -> Tuple_Val (List.map (eval_expr env) es)
  | Dict_Expr pairs ->
      let d = Hashtbl.create 20 in
      let rec fill ps =
        match ps with
        | [] -> ()
        | (k_expr, v_expr)::rest ->
            let k_val = eval_expr env k_expr in
            let v_val = eval_expr env v_expr in
            let k = dict_key_of_value k_val in
            let _ = Hashtbl.replace d k v_val in
            fill rest
      in
      let _ = fill pairs in
      Dict_Val d
  | Index_Expr (e1, e2) -> value_at_index (eval_expr env e1) (eval_expr env e2)
  | Slice_Expr (e1, start_opt, stop_opt, step_opt) ->
      let base_v = eval_expr env e1 in
      let eval_opt opt =
        match opt with
        | None ->
            None
        | Some e1 ->
            Some (eval_expr env e1)
      in
      value_at_slice base_v (eval_opt start_opt) (eval_opt stop_opt) (eval_opt step_opt)
  | Neg_Expr e1 -> neg_value (eval_expr env e1)
  | Add_Expr (e1, e2) -> add_values (eval_expr env e1) (eval_expr env e2)
  | Sub_Expr (e1, e2) -> sub_values (eval_expr env e1) (eval_expr env e2)
  | Pow_Expr (e1, e2) -> pow_values (eval_expr env e1) (eval_expr env e2)
  | Mul_Expr (e1, e2) -> mul_values (eval_expr env e1) (eval_expr env e2)
  | Div_Expr (e1, e2) -> div_values (eval_expr env e1) (eval_expr env e2)
  | Bitand_Expr (e1, e2) -> bitand_values (eval_expr env e1) (eval_expr env e2)
  | Bitor_Expr (e1, e2) -> bitor_values (eval_expr env e1) (eval_expr env e2)
  | Bitxor_Expr (e1, e2) -> bitxor_values (eval_expr env e1) (eval_expr env e2)
  | Lshift_Expr (e1, e2) -> lshift_values (eval_expr env e1) (eval_expr env e2)
  | Rshift_Expr (e1, e2) -> rshift_values (eval_expr env e1) (eval_expr env e2)
  | Is_Eq_Expr (e1, e2) -> eq_values (eval_expr env e1) (eval_expr env e2)
  | Is_Neq_Expr (e1, e2) -> neq_values (eval_expr env e1) (eval_expr env e2)
  | Lt_Expr (e1, e2) -> lt_values (eval_expr env e1) (eval_expr env e2)
  | Leq_Expr (e1, e2) -> leq_values (eval_expr env e1) (eval_expr env e2)
  | Gt_Expr (e1, e2) -> gt_values (eval_expr env e1) (eval_expr env e2)
  | Geq_Expr (e1, e2) -> geq_values (eval_expr env e1) (eval_expr env e2)
  | And_Expr (e1, e2) -> and_values (eval_expr env e1) (eval_expr env e2)
  | Or_Expr (e1, e2) -> or_values (eval_expr env e1) (eval_expr env e2)
  | Not_Expr e1 -> not_value (eval_expr env e1)
  | Bitnot_Expr e1 -> bitnot_value (eval_expr env e1)
  | Range_Expr args -> eval_range_expr env args
  | Call_Expr (fn_expr, arg_exprs) ->
      let fn_v = eval_expr env fn_expr in
      let arg_vs = List.map (eval_expr env) arg_exprs in
      (match fn_v with
       | Function_Val fn ->
           let expected = List.length fn.params in
           let got = List.length arg_vs in
           if expected <> got then
             raise
               (Runtime_Error
                  ("wrong number of arguments: expected "
                   ^ string_of_int expected
                   ^ " but got "
                   ^ string_of_int got))
           else
             let call_env = mk_child_env fn.closure in
             let _ =
               (match fn.name with
                | None -> ()
                | Some name -> define call_env name fn_v)
             in
             let _ =
               List.iter2
                 (fun param arg_v -> define call_env param arg_v)
                 fn.params
                 arg_vs
             in
             (try
                let _ = eval_stmt_list call_env fn.body in
                None_Val
              with
              | Return_Signal ret_v -> ret_v)
       | _ ->
           raise (Runtime_Error ("value is not callable: " ^ string_of_value fn_v)))

and
  eval_range_expr env args =
  let int_arg e =
    require_int_value (eval_expr env e)
  in
  let (start_i, stop_i, step_i) =
    match args with
    | [stop_e] ->
        (0, int_arg stop_e, 1)
    | [start_e; stop_e] ->
        (int_arg start_e, int_arg stop_e, 1)
    | [start_e; stop_e; step_e] ->
        (int_arg start_e, int_arg stop_e, int_arg step_e)
    | _ ->
        raise (Runtime_Error "range expects 1 to 3 integer arguments")
  in
  if step_i = 0 then
    raise (Runtime_Error "range() arg 3 must not be zero")
  else
    let rec build cur acc =
      if (step_i > 0 && cur >= stop_i) || (step_i < 0 && cur <= stop_i) then
        List.rev acc
      else
        build (cur + step_i) (Int_Val cur::acc)
    in
    List_Val (build start_i [])

and iterable_items v =
  match v with
  | List_Val xs ->
      xs
  | Tuple_Val xs ->
      xs
  | String_Val s ->
      let rec chars i acc =
        if i < 0 then
          acc
        else
          let ch = String.make 1 s.[i] in
          chars (i - 1) (String_Val ch::acc)
      in
      chars (String.length s - 1) []
  | Dict_Val d ->
      let keys =
        Hashtbl.fold (fun k _ acc -> k::acc) d []
        |> List.sort (fun k1 k2 -> String.compare (string_of_dict_key k1) (string_of_dict_key k2))
      in
      List.map value_of_dict_key keys
  | _ ->
      raise (Runtime_Error ("value is not iterable: " ^ string_of_value v))

and eval_augassign env name op rhs_expr =
  let lhs = lookup env name in
  let rhs = eval_expr env rhs_expr in
  let result =
    match op with
    | Add_Augop -> add_values lhs rhs
    | Sub_Augop -> sub_values lhs rhs
    | Mul_Augop -> mul_values lhs rhs
    | Div_Augop -> div_values lhs rhs
    | Pow_Augop -> pow_values lhs rhs
    | Bitand_Augop -> bitand_values lhs rhs
    | Bitor_Augop -> bitor_values lhs rhs
    | Bitxor_Augop -> bitxor_values lhs rhs
    | Lshift_Augop -> lshift_values lhs rhs
    | Rshift_Augop -> rshift_values lhs rhs
  in
  update env name result

and eval_stmt_inner env s =
  match s with
  | Exit_Stmt -> raise Exit
  | Expr_Stmt e ->
      let v = eval_expr env e in
      print_endline (string_of_value v)
  | Print_Stmt e ->
      let v = eval_expr env e in
      print_endline (string_of_value v)
  | Assign_Stmt (names, e) ->
      let v = eval_expr env e in
      List.iter (fun name -> define env name v) names
  | Target_Assign_Stmt (target, e) ->
      let v = eval_expr env e in
      assign_target env target v
  | Augassign_Stmt (name, op, e) ->
      eval_augassign env name op e
  | Def_Stmt (name, params, body) ->
      let fn =
        Function_Val
          { name = Some name
          ; params
          ; body
          ; closure = env
          }
      in
      define env name fn
  | Return_Stmt value_opt ->
      (match value_opt with
       | None -> raise (Return_Signal None_Val)
       | Some e -> raise (Return_Signal (eval_expr env e)))
  | If_Stmt (condition, then_body, else_body) ->
      if to_bool_value (eval_expr env condition) then
        eval_stmt_list env then_body
      else
        (match else_body with
         | No_Else -> ()
         | Else_Block stmts -> eval_stmt_list env stmts)
  | While_Stmt (condition, body) ->
      while to_bool_value (eval_expr env condition) do
        eval_stmt_list env body
      done
  | For_Stmt (name, iterable_expr, body) ->
      let items = iterable_items (eval_expr env iterable_expr) in
      List.iter
        (fun item ->
           let _ = update env name item in
           eval_stmt_list env body)
        items

and eval_stmt_list env stmts =
  List.iter (eval_stmt_inner env) stmts
;;

let eval_stmt env s =
  try
    eval_stmt_inner env s
  with
  | Return_Signal _ ->
      raise (Runtime_Error "return outside function")
;;
