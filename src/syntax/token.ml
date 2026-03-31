(* File: token.ml *)

type token = Int_tok of int
           | Bool_tok of bool
           | Float_tok of float
           | String_tok of string
           | Id_tok of string
           | Plus_tok
           | Minus_tok
           | Pow_tok
           | Mult_tok
           | Div_tok
           | Bitand_tok
           | Bitor_tok
           | Bitxor_tok
           | Bitnot_tok
           | Lshift_tok
           | Rshift_tok
           | Is_equ_tok
           | Is_neq_tok
           | Gt_tok
           | Lt_tok
           | Geq_tok
           | Leq_tok
           | Equ_tok
           | And_tok
           | Or_tok
           | Not_tok
           | If_tok
           | Elif_tok
           | Else_tok
           | While_tok
           | Print_tok
           | Lbrace_tok
           | Rbrace_tok
           | Lbracket_tok
           | Rbracket_tok
           | Lparen_tok
           | Rparen_tok
           | Lcomment_tok
           | Rcomment_tok
           | Newline_tok
           | Indent_tok
           | Dedent_tok
           | Comma_tok
           | Colon_tok
;;

exception IgnoreCase;;

let string_of_token tok =
  match tok with
  | Int_tok x      -> "Int_tok " ^ string_of_int x
  | Bool_tok b     -> "Bool_tok " ^ string_of_bool b
  | Float_tok f    -> "Float_tok " ^ string_of_float f
  | String_tok s   -> "String_tok \"" ^ String.escaped s ^ "\""
  | Id_tok s       -> "Id_tok \"" ^ s ^ "\""
  | Plus_tok       -> "Plus_tok"
  | Minus_tok      -> "Minus_tok"
  | Pow_tok        -> "Pow_tok"
  | Mult_tok       -> "Mult_tok"
  | Div_tok        -> "Div_tok"
  | Bitand_tok     -> "Bitand_tok"
  | Bitor_tok      -> "Bitor_tok"
  | Bitxor_tok     -> "Bitxor_tok"
  | Bitnot_tok     -> "Bitnot_tok"
  | Lshift_tok     -> "Lshift_tok"
  | Rshift_tok     -> "Rshift_tok"
  | Is_equ_tok     -> "Is_equ_tok"
  | Is_neq_tok     -> "Is_neq_tok"
  | Gt_tok         -> "Gt_tok"
  | Lt_tok         -> "Lt_tok"
  | Leq_tok        -> "Leq_tok"
  | Geq_tok        -> "Geq_tok"
  | Equ_tok        -> "Equ_tok"
  | If_tok         -> "If_tok"
  | Elif_tok       -> "Elif_tok"
  | Else_tok       -> "Else_tok"
  | While_tok      -> "While_tok"
  | Print_tok      -> "Print_tok"
  | Lbrace_tok     -> "Lbrace_tok"
  | Rbrace_tok     -> "Rbrace_tok"
  | Lbracket_tok   -> "Lbracket_tok"
  | Rbracket_tok   -> "Rbracket_tok"
  | Lparen_tok     -> "Lparen_tok"
  | Rparen_tok     -> "Rparen_tok"
  | Lcomment_tok   -> "Lcomment_tok"
  | Rcomment_tok   -> "Rcomment_tok"
  | Newline_tok    -> "Newline_tok"
  | Indent_tok     -> "Indent_tok"
  | Dedent_tok     -> "Dedent_tok"
  | Comma_tok      -> "Comma_tok"
  | Colon_tok      -> "Colon_tok"
  | And_tok        -> "And_tok"
  | Or_tok         -> "Or_tok"
  | Not_tok        -> "Not_tok"
;;

let string_of_tokens toks =
  let rec aux ts =
    match ts with
    | [] ->
        ""
    | [x] ->
        string_of_token x
    | x::xs ->
        string_of_token x ^ ", " ^ aux xs
  in
  "[" ^ aux toks ^ "]"
;;

let print_token tok =
  print_string (string_of_token tok)
;;

let println_token tok =
  let _ = print_token tok in
  print_char '\n'
;;

let print_tokens toks =
  print_string (string_of_tokens toks)
;;

let println_tokens toks =
  let _ = print_tokens toks in
  print_char '\n'
;;
