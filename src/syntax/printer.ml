open Ast

let quoted_string s =
  "\"" ^ String.escaped s ^ "\""
;;

let rec string_of_expr e =
  match e with
  | Int_Expr n ->
      string_of_int n
  | Float_Expr f ->
      string_of_float f
  | Bool_Expr true ->
      "True"
  | Bool_Expr false ->
      "False"
  | String_Expr s ->
      quoted_string s
  | Var_Expr s ->
      s
  | List_Expr es ->
      "[" ^ string_of_exprs es ^ "]"
  | Tuple_Expr [] ->
      "()"
  | Tuple_Expr [e1] ->
      "(" ^ string_of_expr e1 ^ ",)"
  | Tuple_Expr es ->
      "(" ^ string_of_exprs es ^ ")"
  | Dict_Expr pairs ->
      "{" ^ string_of_dict_pairs pairs ^ "}"
  | Index_Expr (e1, e2) ->
      string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | Neg_Expr e1 ->
      "(-" ^ string_of_expr e1 ^ ")"
  | Add_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Sub_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"
  | Mul_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Div_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " / " ^ string_of_expr e2 ^ ")"
  | Pow_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " ** " ^ string_of_expr e2 ^ ")"
  | Bitand_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " & " ^ string_of_expr e2 ^ ")"
  | Bitor_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " | " ^ string_of_expr e2 ^ ")"
  | Bitxor_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " ^ " ^ string_of_expr e2 ^ ")"
  | Lshift_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " << " ^ string_of_expr e2 ^ ")"
  | Rshift_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " >> " ^ string_of_expr e2 ^ ")"
  | Is_Eq_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " == " ^ string_of_expr e2 ^ ")"
  | Is_Neq_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " != " ^ string_of_expr e2 ^ ")"
  | Lt_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " < " ^ string_of_expr e2 ^ ")"
  | Leq_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " <= " ^ string_of_expr e2 ^ ")"
  | Gt_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " > " ^ string_of_expr e2 ^ ")"
  | Geq_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " >= " ^ string_of_expr e2 ^ ")"
  | And_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " and " ^ string_of_expr e2 ^ ")"
  | Or_Expr (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " or " ^ string_of_expr e2 ^ ")"
  | Not_Expr e1 ->
      "(not " ^ string_of_expr e1 ^ ")"
  | Bitnot_Expr e1 ->
      "(~" ^ string_of_expr e1 ^ ")"

and
  string_of_exprs es =
    match es with
    | [] ->
        ""
    | [e1] ->
        string_of_expr e1
    | e1::rest ->
        string_of_expr e1 ^ ", " ^ string_of_exprs rest

and
  string_of_dict_pairs pairs =
    match pairs with
    | [] ->
        ""
    | [(k, v)] ->
        string_of_expr k ^ ": " ^ string_of_expr v
    | (k, v)::rest ->
        string_of_expr k ^ ": " ^ string_of_expr v ^ ", " ^ string_of_dict_pairs rest
;;

let rec string_of_target t =
  match t with
  | Name_Target name ->
      name
  | Index_Target (t1, e) ->
      string_of_target t1 ^ "[" ^ string_of_expr e ^ "]"
;;

let string_of_string_list xs =
  let rec aux ys =
    match ys with
    | [] -> ""
    | [y] -> y
    | y::ys1 -> y ^ " = " ^ aux ys1
  in
  aux xs
;;

let string_of_augop op =
  match op with
  | Add_Augop -> "+="
  | Sub_Augop -> "-="
  | Mul_Augop -> "*="
  | Div_Augop -> "/="
  | Pow_Augop -> "**="
  | Bitand_Augop -> "&="
  | Bitor_Augop -> "|="
  | Bitxor_Augop -> "^="
  | Lshift_Augop -> "<<="
  | Rshift_Augop -> ">>="
;;

let rec string_of_stmt s =
  match s with
  | Expr_Stmt e ->
      string_of_expr e
  | Print_Stmt e ->
      "print(" ^ string_of_expr e ^ ")"
  | Assign_Stmt (names, e) ->
      string_of_string_list names ^ " = " ^ string_of_expr e
  | Target_Assign_Stmt (target, e) ->
      string_of_target target ^ " = " ^ string_of_expr e
  | Augassign_Stmt (name, op, e) ->
      name ^ " " ^ string_of_augop op ^ " " ^ string_of_expr e
  | Exit_Stmt ->
      "exit()"
  | If_Stmt (cond, then_body, else_part) ->
      let then_s =
        String.concat "; " (List.map string_of_stmt then_body)
      in
      (match else_part with
       | No_Else ->
           "if " ^ string_of_expr cond ^ ": [" ^ then_s ^ "]"
       | Else_Block else_body ->
           let else_s =
             String.concat "; " (List.map string_of_stmt else_body)
           in
           "if " ^ string_of_expr cond ^ ": [" ^ then_s ^ "] else: [" ^ else_s ^ "]")
  | While_Stmt (cond, body) ->
      let body_s =
        String.concat "; " (List.map string_of_stmt body)
      in
      "while " ^ string_of_expr cond ^ ": [" ^ body_s ^ "]"
;;
