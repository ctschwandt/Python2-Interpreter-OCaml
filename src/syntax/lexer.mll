{
    open Token

    exception LexError of string

    let tab_width = 8

    let next_tab_stop n =
        ((n / tab_width) + 1) * tab_width
    ;; (* go to next position that is a multiple of tab width *)

    let indent_stack : int Stack.t = Stack.create ()
    let () = Stack.push 0 indent_stack
    ;; (* indentation level starts at 0 *)

    let reset_indent_stack () =
        Stack.clear indent_stack;
        Stack.push 0 indent_stack
    ;;

    let rec pop_to_level target =
        let cur = Stack.top indent_stack in
        if cur = target then
           []
        else if cur > target then
           let _ = Stack.pop indent_stack in
           Dedent_tok::(pop_to_level target)
        else
           raise (LexError
           ("Indentation error: unindent does not match any outer indentation level"))
    ;;

    let rec emit_eof_dedents () =
        if Stack.length indent_stack <= 1 then
           []
        else
           let _ = Stack.pop indent_stack in
           Dedent_tok::(emit_eof_dedents ())
    ;;

    let apply_indent n =
        let cur = Stack.top indent_stack in
        if n = cur then
           []
        else if n > cur then
           let _ = Stack.push n indent_stack in
           [Indent_tok]
        else
           pop_to_level n
    ;;

let decode_string_literal s =
    let n = String.length s in
    if n < 2 then
      raise (LexError "Invalid string literal")
    else
      let rec loop i acc =
        if i >= n - 1 then
          acc
        else
          let c = s.[i] in
          if c = '\\' then
            if i + 1 >= n - 1 then
              raise (LexError "Invalid escape sequence in string literal")
            else
              let c2 = s.[i + 1] in
              let decoded =
                match c2 with
                | '\\' -> "\\"
                | '\'' -> "'"
                | '"' -> "\""
                | 'n' -> "\n"
                | 't' -> "\t"
                | 'r' -> "\r"
                | _ ->
                    raise
                      (LexError
                         ("Unsupported escape sequence \\"
                          ^ String.make 1 c2))
              in
              loop (i + 2) (acc ^ decoded)
          else
            loop (i + 1) (acc ^ String.make 1 c)
      in
      loop 1 ""
;;
}

let digit = ['0'-'9']
let digits = digit+
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper
let alphanum = letter | digit
let integer = '0' | ['1'-'9'] ['0'-'9']*
let exponent = ['e' 'E'] ['+' '-']? digits
let float = (integer '.' digits exponent?) | (integer exponent)

let dq_char = [^ '"' '\\' '\n' '\r']
let sq_char = [^ '\'' '\\' '\n' '\r']
let escape = '\\' ['\\' '\'' '"' 'n' 't' 'r']
let double_string = '"' (dq_char | escape)* '"'
let single_string = '\'' (sq_char | escape)* '\''
let string_lit = double_string | single_string

let plus = '+'
let minus = '-'
let bool = "False" | "True"
let identifier = (letter | '_') (alphanum | '_')*
let pow = "**"
let pow_equ = "**="
let mult = '*'
let mult_equ = "*="
let div = '/'
let div_equ = "/="
let bitand = '&'
let bitand_equ = "&="
let bitor = '|'
let bitor_equ = "|="
let bitxor = '^'
let bitxor_equ = "^="
let bitnot = '~'
let lshift = "<<"
let lshift_equ = "<<="
let rshift = ">>"
let rshift_equ = ">>="
let plus_equ = "+="
let minus_equ = "-="
let is_equ = "=="
let is_neq = "!="
let lt = '<'
let gt = '>'
let leq = "<="
let geq = ">="
let equ = '='
let and_ = "and"
let or_ = "or"
let not_ = "not"
let if_ = "if"
let else_ = "else"
let elif = "elif"
let while_ = "while"
let print_ = "print"
let colon = ':'
let lbrace = '{'
let rbrace = '}'
let lbracket = '['
let rbracket = ']'
let comma = ','
let lparen = '('
let rparen = ')'
let lcomment = "(*"
let rcomment = "*)"

rule bol n = parse
     | ' '              { bol (n + 1) lexbuf }
     | '\t'             { bol (next_tab_stop n) lexbuf }
     | '\r'             { bol n lexbuf }
     | '\n'             { Lexing.new_line lexbuf; bol 0 lexbuf }
     | lcomment         { comment_bol n 0 lexbuf }

     | float as s       { let pre = apply_indent n in pre @ ((Float_tok (float_of_string s))::(lexer lexbuf)) }
     | integer as s     { let pre = apply_indent n in pre @ ((Int_tok (int_of_string s))::(lexer lexbuf)) }
     | bool as s        { let pre = apply_indent n in pre @ ((Bool_tok (s = "True"))::(lexer lexbuf)) }
     | string_lit as s  { let pre = apply_indent n in pre @ ((String_tok (decode_string_literal s))::(lexer lexbuf)) }
     | if_              { let pre = apply_indent n in pre @ (If_tok::(lexer lexbuf)) }
     | else_            { let pre = apply_indent n in pre @ (Else_tok::(lexer lexbuf)) }
     | elif             { let pre = apply_indent n in pre @ (Elif_tok::(lexer lexbuf)) }
     | and_             { let pre = apply_indent n in pre @ (And_tok::(lexer lexbuf)) }
     | or_              { let pre = apply_indent n in pre @ (Or_tok::(lexer lexbuf)) }
     | not_             { let pre = apply_indent n in pre @ (Not_tok::(lexer lexbuf)) }
     | while_           { let pre = apply_indent n in pre @ (While_tok::(lexer lexbuf)) }
     | print_           { let pre = apply_indent n in pre @ (Print_tok::(lexer lexbuf)) }
     | identifier as s  { let pre = apply_indent n in pre @ ((Id_tok s)::(lexer lexbuf)) }
     | plus             { let pre = apply_indent n in pre @ (Plus_tok::(lexer lexbuf)) }
     | minus            { let pre = apply_indent n in pre @ (Minus_tok::(lexer lexbuf)) }
     | plus_equ         { let pre = apply_indent n in pre @ (Plus_Equ_tok::(lexer lexbuf)) }
     | minus_equ        { let pre = apply_indent n in pre @ (Minus_Equ_tok::(lexer lexbuf)) }
     | pow_equ          { let pre = apply_indent n in pre @ (Pow_Equ_tok::(lexer lexbuf)) }
     | mult_equ         { let pre = apply_indent n in pre @ (Mult_Equ_tok::(lexer lexbuf)) }
     | div_equ          { let pre = apply_indent n in pre @ (Div_Equ_tok::(lexer lexbuf)) }
     | bitand_equ       { let pre = apply_indent n in pre @ (Bitand_Equ_tok::(lexer lexbuf)) }
     | bitor_equ        { let pre = apply_indent n in pre @ (Bitor_Equ_tok::(lexer lexbuf)) }
     | bitxor_equ       { let pre = apply_indent n in pre @ (Bitxor_Equ_tok::(lexer lexbuf)) }
     | lshift_equ       { let pre = apply_indent n in pre @ (Lshift_Equ_tok::(lexer lexbuf)) }
     | rshift_equ       { let pre = apply_indent n in pre @ (Rshift_Equ_tok::(lexer lexbuf)) }
     | pow              { let pre = apply_indent n in pre @ (Pow_tok::(lexer lexbuf)) }
     | mult             { let pre = apply_indent n in pre @ (Mult_tok::(lexer lexbuf)) }
     | div              { let pre = apply_indent n in pre @ (Div_tok::(lexer lexbuf)) }
     | bitand           { let pre = apply_indent n in pre @ (Bitand_tok::(lexer lexbuf)) }
     | bitor            { let pre = apply_indent n in pre @ (Bitor_tok::(lexer lexbuf)) }
     | bitxor           { let pre = apply_indent n in pre @ (Bitxor_tok::(lexer lexbuf)) }
     | bitnot           { let pre = apply_indent n in pre @ (Bitnot_tok::(lexer lexbuf)) }
     | lshift           { let pre = apply_indent n in pre @ (Lshift_tok::(lexer lexbuf)) }
     | rshift           { let pre = apply_indent n in pre @ (Rshift_tok::(lexer lexbuf)) }
     | is_equ           { let pre = apply_indent n in pre @ (Is_equ_tok::(lexer lexbuf)) }
     | is_neq           { let pre = apply_indent n in pre @ (Is_neq_tok::(lexer lexbuf)) }
     | gt               { let pre = apply_indent n in pre @ ((Gt_tok)::(lexer lexbuf)) }
     | lt               { let pre = apply_indent n in pre @ ((Lt_tok)::(lexer lexbuf)) }
     | leq              { let pre = apply_indent n in pre @ ((Leq_tok)::(lexer lexbuf)) }
     | geq              { let pre = apply_indent n in pre @ ((Geq_tok)::(lexer lexbuf)) }
     | equ              { let pre = apply_indent n in pre @ (Equ_tok::(lexer lexbuf)) }
     | lbrace           { let pre = apply_indent n in pre @ (Lbrace_tok::(lexer lexbuf)) }
     | rbrace           { let pre = apply_indent n in pre @ (Rbrace_tok::(lexer lexbuf)) }
     | lparen           { let pre = apply_indent n in pre @ (Lparen_tok::(lexer lexbuf)) }
     | rparen           { let pre = apply_indent n in pre @ (Rparen_tok::(lexer lexbuf)) }
     | lbracket         { let pre = apply_indent n in pre @ (Lbracket_tok::(lexer lexbuf)) }
     | rbracket         { let pre = apply_indent n in pre @ (Rbracket_tok::(lexer lexbuf)) }
     | comma            { let pre = apply_indent n in pre @ (Comma_tok::(lexer lexbuf)) }
     | colon            { let pre = apply_indent n in pre @ (Colon_tok::(lexer lexbuf)) }

     | eof              { pop_to_level 0 }

     | _ as c           { raise (LexError ("Unexpected character '" ^ String.make 1 c ^ "' at beginning of line")) }

and
lexer = parse
     | ' '              { lexer lexbuf }
     | '\t'             { lexer lexbuf }
     | '\r'             { lexer lexbuf }
     | '\n'             { let _ = (Lexing.new_line lexbuf) in (Newline_tok)::(bol 0 lexbuf) }

     | float as s       { (Float_tok (float_of_string s))::(lexer lexbuf) }
     | integer as s     { (Int_tok (int_of_string s))::(lexer lexbuf) }
     | bool as s        { (Bool_tok (s = "True"))::(lexer lexbuf) }
     | string_lit as s  { (String_tok (decode_string_literal s))::(lexer lexbuf) }
     | if_              { (If_tok)::(lexer lexbuf) }
     | elif             { (Elif_tok)::(lexer lexbuf) }
     | else_            { (Else_tok)::(lexer lexbuf) }
     | and_             { And_tok::(lexer lexbuf) }
     | or_              { Or_tok::(lexer lexbuf) }
     | not_             { Not_tok::(lexer lexbuf) }
     | while_           { While_tok::(lexer lexbuf) }
     | print_           { Print_tok::(lexer lexbuf) }
     | identifier as s  { (Id_tok s)::(lexer lexbuf) }
     | plus             { (Plus_tok)::(lexer lexbuf) }
     | minus            { (Minus_tok)::(lexer lexbuf) }
     | plus_equ         { (Plus_Equ_tok)::(lexer lexbuf) }
     | minus_equ        { (Minus_Equ_tok)::(lexer lexbuf) }
     | pow_equ          { (Pow_Equ_tok)::(lexer lexbuf) }
     | mult_equ         { (Mult_Equ_tok)::(lexer lexbuf) }
     | div_equ          { (Div_Equ_tok)::(lexer lexbuf) }
     | bitand_equ       { (Bitand_Equ_tok)::(lexer lexbuf) }
     | bitor_equ        { (Bitor_Equ_tok)::(lexer lexbuf) }
     | bitxor_equ       { (Bitxor_Equ_tok)::(lexer lexbuf) }
     | lshift_equ       { (Lshift_Equ_tok)::(lexer lexbuf) }
     | rshift_equ       { (Rshift_Equ_tok)::(lexer lexbuf) }
     | pow              { (Pow_tok)::(lexer lexbuf) }
     | mult             { (Mult_tok)::(lexer lexbuf) }
     | div              { (Div_tok)::(lexer lexbuf) }
     | bitand           { (Bitand_tok)::(lexer lexbuf) }
     | bitor            { (Bitor_tok)::(lexer lexbuf) }
     | bitxor           { (Bitxor_tok)::(lexer lexbuf) }
     | bitnot           { (Bitnot_tok)::(lexer lexbuf) }
     | lshift           { (Lshift_tok)::(lexer lexbuf) }
     | rshift           { (Rshift_tok)::(lexer lexbuf) }
     | is_equ           { (Is_equ_tok)::(lexer lexbuf) }
     | is_neq           { (Is_neq_tok)::(lexer lexbuf) }
     | gt               { (Gt_tok)::(lexer lexbuf) }
     | lt               { (Lt_tok)::(lexer lexbuf) }
     | leq              { (Leq_tok)::(lexer lexbuf) }
     | geq              { (Geq_tok)::(lexer lexbuf) }
     | equ              { (Equ_tok)::(lexer lexbuf) }
     | lbrace           { (Lbrace_tok)::(lexer lexbuf) }
     | rbrace           { (Rbrace_tok)::(lexer lexbuf) }
     | lparen           { (Lparen_tok)::(lexer lexbuf) }
     | rparen           { (Rparen_tok)::(lexer lexbuf) }
     | lbracket         { (Lbracket_tok)::(lexer lexbuf) }
     | rbracket         { (Rbracket_tok)::(lexer lexbuf) }
     | comma            { (Comma_tok)::(lexer lexbuf) }
     | lcomment         { comment 0 lexbuf }
     | colon            { (Colon_tok)::(lexer lexbuf) }
     | eof              { emit_eof_dedents () }
     | _ as c           { raise (LexError ("Unexpected character '" ^ String.make 1 c ^ "'")) }

and
comment n = parse
     | lcomment         { comment (n + 1) lexbuf }
     | rcomment         { if n = 0 then lexer lexbuf else comment (n - 1) lexbuf }
     | '\n'             { let _ = Lexing.new_line lexbuf in comment n lexbuf }
     | eof              { raise (LexError "Unterminated comment") }
     | _                { comment n lexbuf }

and
comment_bol indent n = parse
     | lcomment         { comment_bol indent (n + 1) lexbuf }
     | rcomment         { if n = 0 then bol indent lexbuf else comment_bol indent (n - 1) lexbuf }
     | '\n'             { let _ = (Lexing.new_line lexbuf) in comment_bol indent n lexbuf }
     | eof              { raise (LexError "Unterminated comment") }
     | _                { comment_bol indent n lexbuf }

{
    let tokenize s =
        let _ = reset_indent_stack () in
        bol 0 (Lexing.from_string s)
    ;;
}
