(* No argument starts the REPL; one filename executes a source file. *)
let read_file filename =
  let channel = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))
;;

let run_file filename =
  let source = read_file filename in
  let statements = Parser.parse_program (Lexer.tokenize source) in
  let env = Env.mk_env () in
  List.iter (Eval.eval_stmt env) statements
;;

let fail kind message =
  prerr_endline (kind ^ ": " ^ message);
  exit 1
;;

let () =
  match Array.to_list Sys.argv with
  | [_] ->
      Repl.run ()
  | [_; filename] ->
      (try
         run_file filename
       with
       | Eval.Exit -> ()
       | Sys_error msg -> fail "File error" msg
       | Lexer.LexError msg -> fail "Lex error" msg
       | Parser.Parse_error msg -> fail "Parse error" msg
       | Eval.Runtime_Error msg -> fail "Runtime error" msg
       | Failure msg -> fail "Failure" msg)
  | _ ->
      prerr_endline "Usage: main.exe [program.py]";
      exit 2
;;
