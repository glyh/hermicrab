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

let decimal_tok = '-'? ('0' | ['1' - '9'] ['0' - '9']*)

let bin_digit = ('0' | '1')
let bin_tok = "0b" (bin_digit | '_')* bin_digit

let oct_digit = ['0' - '7']
let oct_tok = "0x" (oct_digit | '_')* oct_digit

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
  (* TODO: Support f-string *)
  | "\\\\\\" (* zig style multiline string *)
    { 
      multiline_string_part (Buffer.create 17) lexbuf
    }
  (* TODO: parse sigils *)
  (* | '~' { sigil lexbuf } *)

  | "()" { UNIT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "in" { IN }
  | "&&" { AND_THEN }
  | "||" { OR_ELSE }
  | '=' { ASSIGN }
  | '|' { PIPE }
  | ',' { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '%' { REM }
  | "==" { EQ }
  | "!=" { NE }
  | "<=" { LE }
  | "<" { LT }
  | ">=" { GE }
  | '>' { GT }
  | "and" { AND }
  | "or" { OR }
  | "shl" { BSHIFTL }
  | "shr" { BSHIFTR }
  | "bxor" { BXOR }
  | "band" { BAND }
  | "bor" { BOR }
  | "++" { CONCAT }
  | ";" { SEMICOLON }

  (* lex identifiers last, so keywords are not lexed as identifiers *)
  | word as word { WORD word }

  (* no match? raise exception *)
  | _ as c { illegal c }

(* allow nested comments, like OCaml *)
and nestable_comment nesting = parse
  | "#|"
    { nestable_comment (nesting + 1) lexbuf }
  | newline { Lexing.new_line lexbuf; nestable_comment nesting lexbuf }
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

and multiline_string_part buf = parse
  | newline 
    { MULTISTR_PART (Buffer.contents buf) }
  | eof 
    { MULTISTR_PART (Buffer.contents buf) }
  | _ 
    { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      multiline_string_part buf lexbuf
    }
and raw_string buf = parse
  | newline 
    { failwith "[lexer] raw string literal spans across line" }
  | eof 
    { failwith "[lexer] unterminated raw string literal at EOF" }
  | '\\' '\\' { Buffer.add_char buf '\\'; raw_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; raw_string buf lexbuf }
  | '\'' { LIT_STR (Buffer.contents buf) }
  | [^ '\'' '\\']+
    { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      raw_string buf lexbuf
    }
