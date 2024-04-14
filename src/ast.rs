use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref BINOP_MAP: HashMap<Token<'static>, BinOp> = {
        let mut m = HashMap::new();
        m.insert(Token::PIPE, BinOp::Pipe);
        m.insert(Token::ANDTHEN, BinOp::AndThen);
        m.insert(Token::ORELSE, BinOp::OrElse);

        m.insert(Token::ASSIGN, BinOp::Assign);
        m.insert(Token::PLUS, BinOp::Plus);
        m.insert(Token::MINUS, BinOp::Minus);
        m.insert(Token::MULT, BinOp::Mult);
        m.insert(Token::DIV, BinOp::Div);
        m.insert(Token::REM, BinOp::Rem);
        m.insert(Token::EQ, BinOp::Eq);
        m.insert(Token::NEQ, BinOp::Neq);
        m.insert(Token::LEQ, BinOp::Leq);
        m.insert(Token::GEQ, BinOp::Geq);
        m.insert(Token::LT, BinOp::Lt);
        m.insert(Token::GT, BinOp::Gt);
        m.insert(Token::AND, BinOp::And);
        m.insert(Token::BSHIFTL, BinOp::Bshiftl);
        m.insert(Token::BSHIFTR, BinOp::Bshiftr);
        m.insert(Token::BXOR, BinOp::Bxor);
        m.insert(Token::BAND, BinOp::Band);
        m.insert(Token::CONCAT, BinOp::Concat);

        m
    };
    pub static ref UNOP_MAP: HashMap<Token<'static>, UnOp> = {
        let mut m = HashMap::new();
        m.insert(Token::NOT, UnOp::Not);

        m
    };
}

#[derive(Debug, Eq, PartialEq)]
struct Placeholder(());

use Placeholder as Redirector;

use crate::parser::Token;

#[derive(Debug, Eq, PartialEq)]
pub enum Value<'input> {
    VUnit,
    VInt(i64),
    VStr(String),
    VStrStatic(&'input str),
    ProcLam(Vec<&'input str>, Box<Expr<'input>>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinOp {
    Pipe,
    AndThen,
    OrElse,
    Assign,
    Plus,
    Minus,
    Mult,
    Div,
    Rem,
    Eq,
    Neq,
    Leq,
    Geq,
    Lt,
    Gt,
    And,
    Bshiftl,
    Bshiftr,
    Bxor,
    Band,
    Concat,
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr<'input> {
    Ident(&'input str),
    Val(Value<'input>),
    Unary(&'input UnOp, Box<Expr<'input>>),
    If(Box<Expr<'input>>, Box<Expr<'input>>, Box<Expr<'input>>),
    For(&'input str, Box<Expr<'input>>, Box<Expr<'input>>),
    Block(Vec<Expr<'input>>),
    Binary(Box<Expr<'input>>, &'input BinOp, Box<Expr<'input>>),
    Command(&'input str, Vec<Expr<'input>>, Vec<Expr<'input>>),
    WithRedirection(Box<Expr<'input>>, Redirector),
}

pub const EXPR_PASS: Expr = Expr::Val(Value::VUnit);
