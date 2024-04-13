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
    #[regex(r"[^= \r\n\t\f]+", priority = 1)]
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

    #[test]
    fn parse_simple() {
        assert_eq!(
            try_parse("ls\n\n").unwrap(),
            vec![Expr::Command("ls", vec![], vec![])]
        );
    }
}
