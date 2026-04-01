open Lexer
open Parser
open Eval
open Env

let ends_with_colon s =
  let t = String.trim s in
  let n = String.length t in
  n > 0 && t.[n - 1] = ':'
;;

let is_blank s =
  String.trim s = ""
;;

let normalize_indentation line =
  String.concat "    " (String.split_on_char '\t' line)
;;

let count_leading_spaces s =
  let rec aux i =
    if i < String.length s && s.[i] = ' ' then
      aux (i + 1)
    else
      i
  in
  aux 0
;;

let validate_indent line =
  let n = count_leading_spaces line in
  if n mod 4 <> 0 then
    raise (Failure "Indentation must use multiples of 4 spaces")
;;

let read_normalized_line () =
  let line = read_line () in
  let line = normalize_indentation line in
  let _ = validate_indent line in
  line
;;

let starts_with_word s w =
  let n = String.length s in
  let m = String.length w in
  n >= m
  && String.sub s 0 m = w
  && (n = m || s.[m] = ' ' || s.[m] = ':')
;;

let starts_compound_stmt s =
  let t = String.trim s in
  starts_with_word t "if"
  || starts_with_word t "elif"
  || starts_with_word t "else"
  || starts_with_word t "while"
  || starts_with_word t "for"
;;

let rec read_indented_lines acc =
  print_string "... ";
  flush stdout;
  let line = read_normalized_line () in
  if is_blank line then
    List.rev acc
  else
    read_indented_lines (line :: acc)
;;

let read_block () =
  print_string ">>> ";
  flush stdout;
  let first = read_normalized_line () in
  if is_blank first then
    []
  else if ends_with_colon first || starts_compound_stmt first then
    let rest = read_indented_lines [] in
    first :: rest
  else
    [first]
;;

let block_to_string lines =
  String.concat "\n" lines ^ "\n"
;;

let rec repl env =
  try
    let lines = read_block () in
    if lines = [] then
      repl env
    else
      let input = block_to_string lines in
      let toks = tokenize input in
      let tree = parse toks in
      let _ = eval_stmt env tree in
      repl env
  with
  | End_of_file -> ()
  | Exit -> ()
  | LexError msg ->
      let _ = print_endline ("Lex error: " ^ msg) in
      repl env
  | Runtime_Error msg ->
      let _ = print_endline ("Runtime error: " ^ msg) in
      repl env
  | Parse_error msg ->
      let _ = print_endline ("Parse error: " ^ msg) in
      repl env
  | Failure msg ->
      let _ = print_endline ("Failure: " ^ msg) in
      repl env
;;

let run () =
  let env = mk_env () in
  repl env
;;
