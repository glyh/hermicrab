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
fn as_str(lex: &mut Lexer<Token>) -> Option<String> {
    Some(lex.slice().to_string())
}

#[derive(Logos, Clone, Debug, PartialEq)]
// BUG: comments are not nestable for now
#[logos(skip r"[ \t\f]+|#(\n|[^\|][^\n]*)|#\|([^|]|\|[^#])*\|?\|#", error = LexicalError)]
pub enum Token {
    #[regex(r"\r|\n|\r\n")]
    NL,

    #[regex("-?(0|[1-9][0-9]*)", callback = as_str)]
    DecimalLit(String),

    #[regex("0x[0-9a-fA-F]+", callback = as_str)]
    HexLit(String),

    // #[regex(r"#(\n|[^\|][^\n]*)")]

    // #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_string())]
    // Identifier(String),
    #[regex(r"'([^'\\]|\\['\\])*'", callback = as_str)]
    LiteralString(String),

    // TODO: support templated strings in multiline string
    #[regex(r"\\\\\\[^\n]*", callback = as_str)]
    MultilineStringPart(String),

    // TODO: support templated string to contain arbitrary expression
    #[regex(r#""([^"\\{}]|\\["\\{}$]|\$[a-z]*)*""#, callback = as_str)]
    TemplStr(String),
    // NOTE: the below won't work as it creates conflicts. We may actually need to invoke the
    // parser multiple times.

    // #[regex(r#""([^"\\{}]|\\["\\{}$]|\$[a-z]*)*\$\{"#)]
    // TemplStrHead,
    // #[regex(r#"}([^"\\{}]|\\["\\{}$]|\$[a-z]*)*""#)]
    // TemplStrEnd,
    // #[regex(r#"}([^"\\{}]|\\["\\{}$]|\$[a-z]*)*\$\{"#)]
    // TemplStrMid,
    #[regex(r"~[a-z][a-zA-Z0-9_]*", callback = as_str)]
    SigilIdent(String),

    #[regex(r"~[a-z]\(([^)\\]|\\[)\\])*\)", callback = as_str)]
    SigilParened(String),

    #[regex(r"~[a-z]\[([^]\\]|\\[\]\\])*\]", callback = as_str)]
    SigilBracketed(String),

    #[regex(r"~[a-z]\{([^}\\]|\\[}\\])*\}", callback = as_str)]
    SigilBraced(String),

    #[regex(r"/([^/\\]|\\[\\/])*/[a-z]*", callback = as_str)]
    RegexLike(String),

    #[regex(r"[^ \t\f]+", priority = 0, callback = as_str)]
    Word(String),

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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct WrappedLexer<'input> {
    // instead of an iterator over characters, we have a token iterator
    token_stream: SpannedIter<'input, Token>,
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
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(token, span)| Ok((span.start, token?, span.end)))
    }
}

fn hey() {
    let a = vec![Box::new(Token::IF)];
    // a.into_iter().map(|x| *x).collect;
}
