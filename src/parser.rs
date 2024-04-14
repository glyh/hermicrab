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

    #[token("if")]
    IF,
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

    fn try_parse_verbose(input: &str) -> Option<Vec<Expr>> {
        let lexer = WrappedLexer::new(input);
        let parser = ProgramParser::new();
        let lexer2 = WrappedLexer::new(input);
        for tok in lexer2 {
            println!("{:?}", tok);
        }
        Some(parser.parse(lexer).unwrap())
    }
    fn assert_parse_verbose(input: &str, parsed: &str) {
        assert_eq!(format!("{:?}", try_parse_verbose(input).unwrap()), parsed);
    }

    #[test]
    fn parse_single_line_comment() {
        assert_parse(
            "echo 1 #hello I'm here
            echo 2 #wow\n",
            "[Command(\"echo\", [Val(VStrStatic(\"1\"))], []), Command(\"echo\", [Val(VStrStatic(\"2\"))], [])]",
        );
    }

    // TODO: make it nestable, need to rewrite the lexer.
    #[test]
    fn parse_multi_line_comment() {
        assert_parse(
            " #| hello I'm here with multiline comments |#

            echo 2 #wow\n",
            "[Command(\"echo\", [Val(VStrStatic(\"2\"))], [])]",
        );
    }

    #[test]
    fn parse_exp_simple() {
        assert_parse("(1)\n", "[Val(VInt(1))]");
        assert_parse("(((((((())))))))\n", "[Val(VUnit)]");
        assert_parse(
            "('This ACTUALLY works!')\n",
            "[Val(VStr(\"This ACTUALLY works!\"))]",
        );

        assert_parse("(1 + 1)\n", "[Binary(Val(VInt(1)), Plus, Val(VInt(1)))]");
    }

    #[test]
    fn parse_exp_complex() {
        assert_parse(
            "(1 + 1 * 3 << 3 | 9 ^ 100 and '1' ++ '2' == '12')\n",
            "[Binary(Binary(Binary(Binary(Val(VInt(1)), Plus, Binary(Val(VInt(1)), Mult, Val(VInt(3)))), Bshiftl, Val(VInt(3))), Pipe, Binary(Val(VInt(9)), Bxor, Val(VInt(100)))), And, Binary(Binary(Val(VStr(\"1\")), Concat, Val(VStr(\"2\"))), Eq, Val(VStr(\"12\"))))]",
        );
    }

    #[test]
    fn parse_cmds() {
        assert_parse("ls\n\n", r#"[Command("ls", [], [])]"#);
        assert_parse(
            "cd ..\npwd\n",
            "[Command(\"cd\", [Val(VStrStatic(\"..\"))], []), Command(\"pwd\", [], [])]",
        );
        assert_parse(
            "test -d my.zip && 7z x my.zip || echo no zip file\n",
            "[Binary(Binary(Command(\"test\", [Val(VStrStatic(\"-d\")), Val(VStrStatic(\"my.zip\"))], []), AndThen, Command(\"7z\", [Val(VStrStatic(\"x\")), Val(VStrStatic(\"my.zip\"))], [])), OrElse, Command(\"echo\", [Val(VStrStatic(\"no\")), Val(VStrStatic(\"zip\")), Val(VStrStatic(\"file\"))], []))]",
        );
        assert_parse(
            "ls 1 2 3 'some string' -999 -la\n",
            "[Command(\"ls\", [Val(VStrStatic(\"1\")), Val(VStrStatic(\"2\")), Val(VStrStatic(\"3\")), Val(VStr(\"some string\")), Val(VStrStatic(\"-999\")), Val(VStrStatic(\"-la\"))], [])]",
        );
        assert_parse(
            "ls && 7z && mkfs.ntfs && x86_64-pc-linux-gnu-c++-11\n",
            "[Binary(Binary(Binary(Command(\"ls\", [], []), AndThen, Command(\"7z\", [], [])), AndThen, Command(\"mkfs.ntfs\", [], [])), AndThen, Command(\"x86_64-pc-linux-gnu-c++-11\", [], []))]",
        );
    }

    #[test]
    fn parse_if() {
        assert_parse(
            "if (1 + 1 == 2) { echo yes } else { echo world ends. }",
            "[If(Binary(Binary(Val(VInt(1)), Plus, Val(VInt(1))), Eq, Val(VInt(2))), Block([Command(\"echo\", [Val(VStrStatic(\"yes\"))], [])]), Block([Command(\"echo\", [Val(VStrStatic(\"world\")), Val(VStrStatic(\"ends.\"))], [])]))]",
        );
    }

    #[test]
    fn parse_assign() {
        assert_parse(
            "a = 1 + 1\n",
            "[Binary(Ident(\"a\"), Assign, Binary(Val(VInt(1)), Plus, Val(VInt(1))))]",
        );
    }
}
