use logos::{Lexer, Logos, SpannedIter};
use std::fmt;
use std::num::ParseIntError;

// NOTE:
// See https://lalrpop.github.io/lalrpop/lexer_tutorial/005_external_lib.html
// for a tutorial.
// TODO:
// when rust support derive fields to be constant we may use this:
// use pomsky_macro::pomsky;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(ParseIntError),
    #[default]
    InvalidToken,
}

impl From<ParseIntError> for LexicalError {
    fn from(err: ParseIntError) -> Self {
        LexicalError::InvalidInteger(err)
    }
}

#[derive(Logos, Clone, Debug, PartialEq, Hash, Eq)]
// BUG: comments are not nestable for now
#[logos(skip r"[ \t\f]+|#(\n|[^\|][^\n]*)|#\|([^|]|\|[^#])*\|?\|#", error = LexicalError)]
pub enum Token<'input> {
    #[regex(r"\r|\n|\r\n")]
    NL,

    #[regex("-?(0|[1-9][0-9]*)")]
    DecimalLit(&'input str),

    #[regex("0x[0-9a-fA-F]+")]
    HexLit(&'input str),

    // #[regex(r"#(\n|[^\|][^\n]*)")]

    // #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_string())]
    // Identifier(&'input str),
    #[regex(r"'([^'\\]|\\['\\])*'")]
    LiteralString(&'input str),

    // TODO: support templated strings in multiline string
    #[regex(r"\\\\\\[^\n]*")]
    MultilineStringPart(&'input str),

    // TODO: support templated string to contain arbitrary expression
    #[regex(r#""([^"\\{}]|\\["\\{}$]|\$[a-z]*)*""#)]
    TemplStr(&'input str),
    // NOTE: the below won't work as it creates conflicts. We may actually need to invoke the
    // parser multiple times.

    // #[regex(r#""([^"\\{}]|\\["\\{}$]|\$[a-z]*)*\$\{"#)]
    // TemplStrHead,
    // #[regex(r#"}([^"\\{}]|\\["\\{}$]|\$[a-z]*)*""#)]
    // TemplStrEnd,
    // #[regex(r#"}([^"\\{}]|\\["\\{}$]|\$[a-z]*)*\$\{"#)]
    // TemplStrMid,
    #[regex(r"~[a-z][a-zA-Z0-9_]*")]
    SigilIdent(&'input str),

    #[regex(r"~[a-z]\(([^)\\]|\\[)\\])*\)")]
    SigilParened(&'input str),

    #[regex(r"~[a-z]\[([^]\\]|\\[\]\\])*\]")]
    SigilBracketed(&'input str),

    #[regex(r"~[a-z]\{([^}\\]|\\[}\\])*\}")]
    SigilBraced(&'input str),

    #[regex(r"/([^/\\]|\\[\\/])*/[a-z]*")]
    RegexLike(&'input str),

    // `=` is forbidden because we want assignments
    #[regex(r"[^= \r\n\t\f()]+", priority = 1)]
    Word(&'input str),

    #[token("){")]
    RPARENLBRACE,
    #[token("()")]
    UNIT,
    #[token("(")]
    LPAREN,
    #[token(")")]
    RPAREN,
    #[token("[")]
    LBRACKET,
    #[token("]")]
    RBRACKET,
    #[token("{")]
    LBRACE,
    #[token("}")]
    RBRACE,

    #[token("do")]
    DO,
    #[token("end")]
    END,
    #[token("if")]
    IF,
    #[token("elif")]
    ELIF,
    #[token("else")]
    ELSE,
    #[token("for")]
    FOR,
    #[token("in")]
    IN,
    #[token("&&")]
    ANDTHEN,
    #[token("||")]
    ORELSE,
    #[token("=")]
    ASSIGN,
    #[token("|")]
    PIPE,
    #[token(",")]
    COMMA,
    #[token(":")]
    COLON,
    #[token(";")]
    SEMICOLON,
    #[token("+")]
    PLUS,
    #[token("-")]
    MINUS,
    #[token("*")]
    MULT,
    #[token("/")]
    DIV,
    #[token("%")]
    REM,
    #[token("==")]
    EQ,
    #[token("!=")]
    NEQ,
    #[token("<=")]
    LEQ,
    #[token(">=")]
    GEQ,
    #[token("<")]
    LT,
    #[token(">")]
    GT,
    #[token("and")]
    AND,
    #[token("or")]
    OR,
    #[token("<<")]
    BSHIFTL,
    #[token(">>")]
    BSHIFTR,
    #[token("^")]
    BXOR,
    #[token("&")]
    BAND,
    #[token("++")]
    CONCAT,
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct WrappedLexer<'input> {
    // instead of an iterator over characters, we have a token iterator
    token_stream: SpannedIter<'input, Token<'input>>,
}

impl<'input> WrappedLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        // the Token::lexer() method is provided by the Logos trait
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'input> Iterator for WrappedLexer<'input> {
    type Item = Spanned<Token<'input>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(token, span)| Ok((span.start, token?, span.end)))
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Expr, grammar::ProgramParser, parser::WrappedLexer};

    fn try_parse(input: &str) -> Option<Vec<Expr>> {
        let lexer = WrappedLexer::new(input);
        let parser = ProgramParser::new();
        Some(parser.parse(lexer).unwrap())
    }
    fn assert_parse(input: &str, parsed: &str) {
        assert_eq!(format!("{:?}", try_parse(input).unwrap()), parsed);
    }

    #[test]
    fn parse_cmd_simple() {
        assert_parse("ls\n\n", r#"[Command("ls", [], [])]"#);
        assert_parse(
            "cd ..\npwd\n",
            r#"[Command("cd", [".."], []), Command("pwd", [], [])]"#,
        );
    }

    #[test]
    fn parse_single_line_comment() {
        assert_parse(
            "echo 1 #hello I'm here
            echo 2 #wow\n",
            "[Command(\"echo\", [\"1\"], []), Command(\"echo\", [\"2\"], [])]",
        );
    }

    // TODO: make it nestable, need to rewrite the lexer.
    #[test]
    fn parse_multi_line_comment() {
        assert_parse(
            " #| hello I'm here with multiline comments |#

            echo 2 #wow\n",
            "[Command(\"echo\", [\"2\"], [])]",
        );
    }

    #[test]
    fn parse_exp_simple() {
        assert_parse("(1)\n", "[Val(VInt(1))]");
        assert_parse("(((((((())))))))\n", "[Val(VUnit)]");
        assert_parse(
            "('This ACTUALLY works!')\n",
            "[Val(VStr(\"This ACTUALLY works\"))]",
        );
    }
}

//   let%expect_test "exp-string" =
//     parse_string "('This ACTUALLY works!')"
//     |> Printf.printf !"%{sexp:Ast.program}";
//     [%expect{|  ((Val (String "This ACTUALLY works!")))  |}]
//
//   let%expect_test "exp-simple" =
//     parse_string "(1 + 1)"
//     |> Printf.printf !"%{sexp:Ast.program}";
//     [%expect{|
//       ((BinOp Add (Val (Int 1)) (Val (Int 1))))
//     |}]
//
//   let%expect_test "exp-complex" =
//     parse_string "(1 + 1 * 3 shl 3 | 9 xor 100 and '1' ++ '2' == '12')"
//     |> Printf.printf !"%{sexp:Ast.program}";
//     [%expect{|
//       ((BinOp Land
//         (BinOp BOr
//          (BinOp BShiftL
//           (BinOp Add (Val (Int 1)) (BinOp Mul (Val (Int 1)) (Val (Int 3))))
//           (Val (Int 3)))
//          (BinOp BXor (Val (Int 9)) (Val (Int 100))))
//         (BinOp Eq (BinOp Concat (Val (String 1)) (Val (String 2)))
//          (Val (String 12)))))
//     |}]
//
//   let%expect_test "simple cmd" =
//     parse_string "test -d my.zip && 7z x my.zip || echo no zip file"
//     |> Printf.printf !"%{sexp:Ast.program}";
//     [%expect{|
//         ((If
//           (If (Command test ((Word -d) (Word my.zip)) ())
//            (Command 7z ((Word x) (Word my.zip)) ()) (Val (Int 1)))
//           (Val (Unit ())) (Command echo ((Word no) (Word zip) (Word file)) ())))
//     |}]
//
//   let%expect_test "different word types" =
//     parse_string "ls 1 2 3 'some string' -999 -la"
//     |> Printf.printf !"%{sexp:Ast.program}";
//     [%expect{|
//         ((Command ls
//           ((Int 1) (Int 2) (Int 3) (String "some string") (Int -999) (Word -la)) ()))
//     |}]
//
//
//   let%expect_test "weird cmds" =
//     parse_string "ls && 7z && mkfs.ntfs && x86_64-pc-linux-gnu-c++-11"
//     |> Printf.printf !"%{sexp:Ast.program}";
//     [%expect{|
//         ((If
//           (If (If (Command ls () ()) (Command 7z () ()) (Val (Int 1)))
//            (Command mkfs.ntfs () ()) (Val (Int 1)))
//           (Command x86_64-pc-linux-gnu-c++-11 () ()) (Val (Int 1))))
//     |}]
//
//   let%expect_test "if" =
//     parse_string "if (1 + 1 == 2) { echo yes } else { echo world ends. }"
//     |> Printf.printf !"%{sexp:Ast.program}";
//     [%expect{|
//       ((If (BinOp Eq (BinOp Add (Val (Int 1)) (Val (Int 1))) (Val (Int 2)))
//         (Block ((Command echo ((Word yes)) ())))
//         (Block ((Command echo ((Word world) (Word ends.)) ())))))
//     |}]
//
//   let%expect_test "assign" =
//     parse_string "a = 1 + 1"
//     |> Printf.printf !"%{sexp:Ast.program}";
//     [%expect{|
//       ((Assign a (BinOp Add (Val (Int 1)) (Val (Int 1)))))
//     |}]
//
// end)
