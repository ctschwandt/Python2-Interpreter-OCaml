open Ast
open Value
open Env
open Ops

exception Exit
exception Runtime_Error = Value.Runtime_Error

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
  | Neg_Expr e1 -> neg_value (eval_expr env e1)
  | Add_Expr (e1, e2) -> add_values (eval_expr env e1) (eval_expr env e2)
  | Sub_Expr (e1, e2) -> sub_values (eval_expr env e1) (eval_expr env e2)
  | Pow_Expr (e1, e2) -> pow_values (eval_expr env e1) (eval_expr env e2)
  | Mul_Expr (e1, e2) -> mul_values (eval_expr env e1) (eval_expr env e2)
  | Div_Expr (e1, e2) -> div_values (eval_expr env e1) (eval_expr env e2)
  | Is_Eq_Expr (e1, e2) -> eq_values (eval_expr env e1) (eval_expr env e2)
  | Is_Neq_Expr (e1, e2) -> neq_values (eval_expr env e1) (eval_expr env e2)
  | Lt_Expr (e1, e2) -> lt_values (eval_expr env e1) (eval_expr env e2)
  | Leq_Expr (e1, e2) -> leq_values (eval_expr env e1) (eval_expr env e2)
  | Gt_Expr (e1, e2) -> gt_values (eval_expr env e1) (eval_expr env e2)
  | Geq_Expr (e1, e2) -> geq_values (eval_expr env e1) (eval_expr env e2)
  | And_Expr (e1, e2) -> and_values (eval_expr env e1) (eval_expr env e2)
  | Or_Expr (e1, e2) -> or_values (eval_expr env e1) (eval_expr env e2)
  | Not_Expr e1 -> not_value (eval_expr env e1)
;;

let rec eval_stmt env s =
  match s with
  | Exit_Stmt -> raise Exit
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
         | No_Else -> ()
         | Else_Block stmts -> eval_stmt_list env stmts)
  | While_Stmt (condition, body) ->
      while to_bool_value (eval_expr env condition) do
        eval_stmt_list env body
      done

and eval_stmt_list env stmts =
  List.iter (eval_stmt env) stmts
;;
