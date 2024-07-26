{
open Menhir_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

(* regular expressions *)
let WS = [ ' ' '\t' ]
let digit = ['0' - '9']
let newline = "\r\n" | '\r' | '\n'
let _0marker = ['x' 'b' 'o']

(* Word token *)
(* some example: ls -a 7z x86_64-pc-linux-gnu-c++-11 *)
(* some non-example: 0xDEADBEEF 999 -123 (* <- this should be parsed as a int insteaed *)  *) 
(* BUG: for now 9.9 is parsed as a word but in reality it should be a float constant *)
let word_nondigit = ['A'-'Z' 'a'-'z' '_' '.' '+']
let word_char = word_nondigit | digit | '-'
let word = 
    ( word_nondigit word_char* ) 
  | '-' word_char* word_nondigit word_char* 
  (* the below cases are mostly specifically for 7z, fuck you 7z. *)
  | [ '1' - '9' ] word_nondigit word_char*
  | '0' (_ # _0marker) word_char*

let decimal_tok = '-'? ('0' | ['1' - '9' '_'] ['0' - '9' '_']*)

let bin_digit = ('0' | '1')
let bin_tok = "0b" (bin_digit | '_')* bin_digit

let oct_digit = ['0' - '7']
let oct_tok = "0o" (oct_digit | '_')* oct_digit

let hex_digit = ['0' - '9'] | ['a' - 'f'] | ['A' - 'F']
let hex_tok = "0x" (hex_digit | '_')* hex_digit

rule next_token = parse
  | decimal_tok
    { 
      DECIMAL_LIT (Lexing.lexeme lexbuf)
    }
  | bin_tok
    { 
      BIN_LIT (Lexing.lexeme lexbuf)
    }
  | oct_tok
    { 
      OCT_LIT (Lexing.lexeme lexbuf)
    }
  | hex_tok
    { 
      HEX_LIT (Lexing.lexeme lexbuf)
    }
  | eof { EOF }
  | WS+ { next_token lexbuf }
  | newline { Lexing.new_line lexbuf; NL }
  | "#|" { nestable_comment 0 lexbuf; next_token lexbuf }
  | "#" { single_line_comment lexbuf }
  | "'" { raw_string (Buffer.create 17) lexbuf }
  (* template string *)
  | '"' { templ_str_start (Buffer.create 17) lexbuf }
  | "}\"" { templ_str_rest (Buffer.create 17) lexbuf }
 (* zig style multiline string interpolated *)
  | "\"\"\"{"
    {
      MULTISTR_INTERPOL
    }
  | "\"\"\""
    { 
      multiline_string_part (Buffer.create 17) lexbuf
    }
  (* TODO: parse sigils *)
  (* | '~' { sigil lexbuf } *)

  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "in" { IN }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "xor" { XOR }
  | "shl" { BSHIFTL }
  | "shr" { BSHIFTR }
  | "proc" { PROC }
  | "true" { TRUE }
  | "false" { FALSE }
  | "mod" { MOD }
  | "()" { UNIT }
  | "&&" { AND_THEN }
  | "||" { OR_ELSE }
  | "==" { EQ }
  | "!=" { NE }
  | "<=" { LE }
  | ">=" { GE }
  | "++" { CONCAT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '=' { ASSIGN }
  | '|' { PIPE }
  | ',' { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '\\' { FSLASH }
  | '<' { LT }
  | '>' { GT }
  | ';' { SEMICOLON }

  (* lex identifiers last, so keywords are not lexed as identifiers *)
  | word as word { WORD word }

  (* no match? raise exception *)
  | _ as c { illegal c }

(* allow nested comments, like OCaml *)
and nestable_comment nesting = parse
  | "#|"
    { nestable_comment (nesting + 1) lexbuf }
  | newline 
    {
      Lexing.new_line lexbuf;
      nestable_comment nesting lexbuf
    }
  | "|#"
    {
      if (nesting > 0)
        then nestable_comment (nesting - 1) lexbuf
        else ()
    }
  | eof
    { failwith "[lexer] unterminated comment at EOF" }
  | _
    { nestable_comment nesting lexbuf }

and single_line_comment = parse
  | newline 
    { Lexing.new_line lexbuf; NL }
  | eof 
    { EOF }
  | _ 
    { single_line_comment lexbuf }

and raw_string buf = parse
  | newline 
    {
      Lexing.new_line lexbuf;
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      raw_string buf lexbuf
    }
  | eof 
    { failwith "[lexer] unterminated raw string literal at EOF" }
  | '\\' '\\' { Buffer.add_char buf '\\'; raw_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; raw_string buf lexbuf }
  | '\'' { LIT_STR (Buffer.contents buf) }
  | _
    { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      raw_string buf lexbuf
    }

and templ_str_start buf = parse
  | newline 
    {
      Lexing.new_line lexbuf;
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      templ_str_start buf lexbuf
    }
  | eof 
    { failwith "[lexer] unterminated template string at EOF" }
  | '\\' '\\' { Buffer.add_char buf '\\'; templ_str_start buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; templ_str_start buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; templ_str_start buf lexbuf }
  | '\\' '{' { Buffer.add_char buf '{'; templ_str_start buf lexbuf }
  | "\"{" { TEMPL_STR_R (Buffer.contents buf) }
  | '"' { TEMPL_STR (Buffer.contents buf) }
  | _
    { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      templ_str_start buf lexbuf
    }

and templ_str_rest buf = parse
  | newline 
    {
      Lexing.new_line lexbuf;
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      templ_str_rest buf lexbuf
    }
  | eof 
    { failwith "[lexer] unterminated template string at EOF" }
  | '\\' '\\' { Buffer.add_char buf '\\'; templ_str_rest buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; templ_str_rest buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; templ_str_rest buf lexbuf }
  | '\\' '{' { Buffer.add_char buf '{'; templ_str_rest buf lexbuf }
  | "\"{" { TEMPL_STR_LR (Buffer.contents buf) }
  | '"' { TEMPL_STR_L (Buffer.contents buf) }
  | _
    { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      templ_str_start buf lexbuf
    }


and multiline_string_part buf = parse
  | newline 
    { Lexing.new_line lexbuf; MULTISTR_PART (Buffer.contents buf) }
  | eof 
    { MULTISTR_PART (Buffer.contents buf) }
  | _ 
    { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      multiline_string_part buf lexbuf
    }
