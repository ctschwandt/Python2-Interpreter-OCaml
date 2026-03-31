(* file: parser.ml *)

open Token
open Ast

exception Parse_error of string;;

let rec skip_newlines toks =
  match toks with
  | Newline_tok::rest ->
      skip_newlines rest
  | _ ->
      toks
;;

let rec target_of_expr e =
  match e with
  | Var_Expr name ->
      Name_Target name
  | Index_Expr (base, idx) ->
      Index_Target (target_of_expr base, idx)
  | _ ->
      raise
        (Parse_error
           "Left side of assignment must be a variable or indexed variable")
;;

let check_no_unexpected_indent toks =
  match toks with
  | Indent_tok::_ ->
      raise (Parse_error "Indentation error: unexpected indent")
  | Dedent_tok::_ ->
      raise (Parse_error "Indentation error: unexpected dedent")
  | _ ->
      ()
;;

let rec
  assign_targets toks =
    let toks = skip_newlines toks in
    match toks with
    | Id_tok name::Equ_tok::toks1 ->
        let toks1 = skip_newlines toks1 in
        let (names, rest) = assign_targets toks1 in
        (name::names, rest)
    | _ ->
        ([], toks)

and
  stmt toks =
    let toks = skip_newlines toks in
    let _ = check_no_unexpected_indent toks in
    match toks with
    | Id_tok "exit"::Lparen_tok::Rparen_tok::rest ->
        (Exit_Stmt, skip_newlines rest)
    | Id_tok "exit"::rest ->
        (Exit_Stmt, skip_newlines rest)
    | Id_tok "quit"::Lparen_tok::Rparen_tok::rest ->
        (Exit_Stmt, skip_newlines rest)
    | Id_tok "quit"::rest ->
        (Exit_Stmt, skip_newlines rest)
    | If_tok::rest ->
        let (cond, rest1) = full_expr rest in
        (match skip_newlines rest1 with
         | Colon_tok::rest2 ->
             let (then_body, rest3) = stmt_or_block rest2 in
             let (else_part, rest4) = else_part_parse rest3 in
             (If_Stmt(cond, then_body, else_part), rest4)
         | _ ->
             raise (Parse_error "Expected Colon_tok after if condition"))
    | While_tok::rest ->
        let (cond, rest1) = full_expr rest in
        (match skip_newlines rest1 with
         | Colon_tok::rest2 ->
           let (body, rest3) = stmt_or_block rest2 in
           (While_Stmt(cond, body), rest3)
         | _ ->
           raise (Parse_error "Expected Colon_tok after while condition"))
    | Print_tok::Lparen_tok::rest ->
        let (e, rest1) = full_expr rest in
        (match skip_newlines rest1 with
         | Rparen_tok::rest2 ->
             (Print_Stmt e, skip_newlines rest2)
         | _ ->
             raise (Parse_error "Expected Rparen_tok after print expression"))
    | Print_tok::_ ->
        raise (Parse_error "Expected print(expr)")
    | Id_tok _::_ ->
      let (names, rest1) = assign_targets toks in
      (match names with
       | _::_ ->
         let (e, rest2) = full_expr rest1 in
         (Assign_Stmt(names, e), skip_newlines rest2)
       | [] ->
         let (lhs, rest2) = full_expr toks in
         (match skip_newlines rest2 with
          | Equ_tok::rest3 ->
            let target = target_of_expr lhs in
            let (rhs, rest4) = full_expr rest3 in
            (Target_Assign_Stmt(target, rhs), skip_newlines rest4)
          | _ ->
            (Expr_Stmt lhs, skip_newlines rest2)))
    | _ ->
        let (e, rest) = full_expr toks in
        (Expr_Stmt e, skip_newlines rest)

and
  else_part_parse toks =
    let toks = skip_newlines toks in
    match toks with
    | Else_tok::rest1 ->
        (match skip_newlines rest1 with
         | Colon_tok::rest2 ->
             let (else_body, rest3) = stmt_or_block rest2 in
             (Else_Block else_body, rest3)
         | _ ->
             raise (Parse_error "Expected Colon_tok after else"))
    | Elif_tok::rest1 ->
        let (cond, rest2) = full_expr rest1 in
        (match skip_newlines rest2 with
         | Colon_tok::rest3 ->
             let (then_body, rest4) = stmt_or_block rest3 in
             let (else_part, rest5) = else_part_parse rest4 in
             (Else_Block [If_Stmt(cond, then_body, else_part)], rest5)
         | _ ->
             raise (Parse_error "Expected Colon_tok after elif condition"))
    | _ ->
        (No_Else, toks)

and
  stmt_or_block toks =
    let toks = skip_newlines toks in
    match toks with
    | Indent_tok::_ ->
        block toks
    | _ ->
        let (s, rest) = stmt toks in
        ([s], rest)

and
  block toks =
    let toks = skip_newlines toks in
    match toks with
    | Indent_tok::rest ->
        stmt_list rest
    | _ ->
        raise (Parse_error "Expected indented block")

and
  stmt_list toks =
    let toks = skip_newlines toks in
    match toks with
    | Dedent_tok::rest ->
        ([], rest)
    | [] ->
        raise (Parse_error "Expected Dedent_tok to end block")
    | _ ->
        let (s, rest1) = stmt toks in
        let (ss, rest2) = stmt_list rest1 in
        (s::ss, rest2)

and
  full_expr toks =
    let toks = skip_newlines toks in
    let _ = check_no_unexpected_indent toks in
    or_expr toks

and
  or_expr toks =
    let toks = skip_newlines toks in
    let (left, toks1) = and_expr toks in
    or_tail left toks1

and
  or_tail left toks =
    let toks = skip_newlines toks in
    match toks with
    | Or_tok::toks1 ->
        let (right, toks2) = and_expr toks1 in
        or_tail (Or_Expr(left, right)) toks2
    | _ ->
        (left, toks)

and
  and_expr toks =
    let toks = skip_newlines toks in
    let (left, toks1) = not_expr toks in
    and_tail left toks1

and
  and_tail left toks =
    let toks = skip_newlines toks in
    match toks with
    | And_tok::toks1 ->
        let (right, toks2) = not_expr toks1 in
        and_tail (And_Expr(left, right)) toks2
    | _ ->
        (left, toks)

and
  not_expr toks =
    let toks = skip_newlines toks in
    match toks with
    | Not_tok::toks1 ->
        let (e, toks2) = not_expr toks1 in
        (Not_Expr e, toks2)
    | _ ->
        comparison toks

and
  comparison toks =
    let toks = skip_newlines toks in
    let _ = check_no_unexpected_indent toks in
    let (left, toks1) = expr toks in
    comparison_tail left toks1

and
  comparison_tail left toks =
    let toks = skip_newlines toks in
    match toks with
    | Is_equ_tok::toks1 ->
        let (right, toks2) = expr toks1 in
        (Is_Eq_Expr(left, right), toks2)
    | Is_neq_tok::toks1 ->
        let (right, toks2) = expr toks1 in
        (Is_Neq_Expr(left, right), toks2)
    | Lt_tok::toks1 ->
        let (right, toks2) = expr toks1 in
        (Lt_Expr(left, right), toks2)
    | Leq_tok::toks1 ->
        let (right, toks2) = expr toks1 in
        (Leq_Expr(left, right), toks2)
    | Gt_tok::toks1 ->
        let (right, toks2) = expr toks1 in
        (Gt_Expr(left, right), toks2)
    | Geq_tok::toks1 ->
        let (right, toks2) = expr toks1 in
        (Geq_Expr(left, right), toks2)
    | _ ->
        (left, toks)

and
  expr toks =
    let toks = skip_newlines toks in
    let _ = check_no_unexpected_indent toks in
    let (left, toks1) = term toks in
    expr_tail left toks1

and
  expr_tail left toks =
    let toks = skip_newlines toks in
    match toks with
    | Plus_tok::toks1 ->
        let (right, toks2) = term toks1 in
        expr_tail (Add_Expr(left, right)) toks2
    | Minus_tok::toks1 ->
        let (right, toks2) = term toks1 in
        expr_tail (Sub_Expr(left, right)) toks2
    | _ ->
        (left, toks)

and
  term toks =
    let toks = skip_newlines toks in
    let (left, toks1) = power toks in
    term_tail left toks1

and
  term_tail left toks =
    let toks = skip_newlines toks in
    match toks with
    | Mult_tok::toks1 ->
        let (right, toks2) = power toks1 in
        term_tail (Mul_Expr(left, right)) toks2
    | Div_tok::toks1 ->
        let (right, toks2) = power toks1 in
        term_tail (Div_Expr(left, right)) toks2
    | _ ->
        (left, toks)

and
  power toks =
    let toks = skip_newlines toks in
    let (left, toks1) = unary toks in
    match skip_newlines toks1 with
    | Pow_tok::toks2 ->
        let (right, toks3) = power toks2 in
        (Pow_Expr(left, right), toks3)
    | toks2 ->
        (left, toks2)

and
  unary toks =
    let toks = skip_newlines toks in
    match toks with
    | Plus_tok::toks1 ->
        unary toks1
    | Minus_tok::toks1 ->
        let (e, toks2) = unary toks1 in
        (Neg_Expr e, toks2)
    | Indent_tok::_ ->
        raise (Parse_error "Indentation error: unexpected indent")
    | Dedent_tok::_ ->
        raise (Parse_error "Indentation error: unexpected dedent")
    | _ ->
        postfix toks

and
  postfix toks =
    let (base, toks1) = atom toks in
    postfix_tail base toks1

and
  postfix_tail base toks =
    let toks = skip_newlines toks in
    match toks with
    | Lbracket_tok::toks1 ->
        let (index, toks2) = full_expr toks1 in
        (match skip_newlines toks2 with
         | Rbracket_tok::toks3 ->
             postfix_tail (Index_Expr(base, index)) toks3
         | _ ->
             raise (Parse_error "Expected Rbracket_tok after index expression"))
    | _ ->
        (base, toks)

and
  list_literal toks =
    let toks = skip_newlines toks in
    match toks with
    | Rbracket_tok::rest ->
        (List_Expr [], rest)
    | _ ->
        let (e1, rest1) = full_expr toks in
        list_literal_tail [e1] rest1

and
  list_literal_tail rev_items toks =
    let toks = skip_newlines toks in
    match toks with
    | Comma_tok::rest1 ->
        (match skip_newlines rest1 with
         | Rbracket_tok::rest2 ->
             (List_Expr (List.rev rev_items), rest2)
         | _ ->
             let (e, rest2) = full_expr rest1 in
             list_literal_tail (e::rev_items) rest2)
    | Rbracket_tok::rest ->
        (List_Expr (List.rev rev_items), rest)
    | _ ->
        raise (Parse_error "Expected Comma_tok or Rbracket_tok in list literal")

and
  tuple_or_group toks =
    let toks = skip_newlines toks in
    match toks with
    | Rparen_tok::rest ->
        (Tuple_Expr [], rest)
    | _ ->
        let (e1, rest1) = full_expr toks in
        (match skip_newlines rest1 with
         | Comma_tok::rest2 ->
             tuple_literal_tail [e1] rest2
         | Rparen_tok::rest2 ->
             (e1, rest2)
         | _ ->
             raise (Parse_error "Expected Comma_tok or Rparen_tok"))

and
  tuple_literal_tail rev_items toks =
    let toks = skip_newlines toks in
    match toks with
    | Rparen_tok::rest ->
        (Tuple_Expr (List.rev rev_items), rest)
    | _ ->
        let (e, rest1) = full_expr toks in
        (match skip_newlines rest1 with
         | Comma_tok::rest2 ->
             tuple_literal_tail (e::rev_items) rest2
         | Rparen_tok::rest2 ->
             (Tuple_Expr (List.rev (e::rev_items)), rest2)
         | _ ->
             raise (Parse_error "Expected Comma_tok or Rparen_tok in tuple literal"))

and
  dict_literal toks =
    let toks = skip_newlines toks in
    match toks with
    | Rbrace_tok::rest ->
        (Dict_Expr [], rest)
    | _ ->
        let (k1, rest1) = full_expr toks in
        (match skip_newlines rest1 with
         | Colon_tok::rest2 ->
             let (v1, rest3) = full_expr rest2 in
             dict_literal_tail [(k1, v1)] rest3
         | _ ->
             raise (Parse_error "Expected Colon_tok in dict literal"))

and
  dict_literal_tail rev_pairs toks =
    let toks = skip_newlines toks in
    match toks with
    | Comma_tok::rest1 ->
        (match skip_newlines rest1 with
         | Rbrace_tok::rest2 ->
             (Dict_Expr (List.rev rev_pairs), rest2)
         | _ ->
             let (k, rest2) = full_expr rest1 in
             (match skip_newlines rest2 with
              | Colon_tok::rest3 ->
                  let (v, rest4) = full_expr rest3 in
                  dict_literal_tail ((k, v)::rev_pairs) rest4
              | _ ->
                  raise (Parse_error "Expected Colon_tok in dict literal")))
    | Rbrace_tok::rest ->
        (Dict_Expr (List.rev rev_pairs), rest)
    | _ ->
        raise (Parse_error "Expected Comma_tok or Rbrace_tok in dict literal")

and
  atom toks =
    let toks = skip_newlines toks in
    match toks with
    | Int_tok n::rest ->
        (Int_Expr n, rest)
    | Float_tok x::rest ->
        (Float_Expr x, rest)
    | Bool_tok b::rest ->
        (Bool_Expr b, rest)
    | String_tok s::rest ->
        (String_Expr s, rest)
    | Id_tok name::rest ->
        (Var_Expr name, rest)
    | Lbracket_tok::toks1 ->
        list_literal toks1
    | Lparen_tok::toks1 ->
        tuple_or_group toks1
    | Lbrace_tok::toks1 ->
        dict_literal toks1
    | Indent_tok::_ ->
        raise (Parse_error "Indentation error: unexpected indent")
    | Dedent_tok::_ ->
        raise (Parse_error "Indentation error: unexpected dedent")
    | _ ->
        raise
          (Parse_error
             "Expected int, float, bool, string, identifier, list, tuple, dict, or parenthesized expression")

let parse toks =
  let toks = skip_newlines toks in
  let _ = check_no_unexpected_indent toks in
  match stmt toks with
  | (tree, rest) ->
      let rest = skip_newlines rest in
      (match rest with
       | [] ->
           tree
       | Indent_tok::_ ->
           raise (Parse_error "Indentation error: unexpected indent")
       | Dedent_tok::_ ->
           raise (Parse_error "Indentation error: unexpected dedent")
       | _ ->
           raise (Parse_error ("Unconsumed tokens: " ^ string_of_tokens rest)))
;;
