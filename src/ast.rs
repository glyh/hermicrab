use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref BINOP_MAP: HashMap<Token<'static>, BinOp> = {
        let mut m = HashMap::new();
        m.insert(Token::PIPE, BinOp::Pipe);
        m.insert(Token::ANDTHEN, BinOp::AndThen);
        m.insert(Token::ORELSE, BinOp::OrElse);
        m
    };
}

#[derive(Debug)]
struct Placeholder(());

use Placeholder as Redirector;
use Placeholder as Unop;

use crate::parser::Token;

#[derive(Debug)]
pub enum Value<'input> {
    VUnit,
    VInt(i64),
    VStr(String),
    ProcLam(Vec<&'input str>, Box<Expr<'input>>),
}

#[derive(Debug)]
pub enum BinOp {
    Pipe,
    AndThen,
    OrElse,
}

#[derive(Debug)]
pub enum Expr<'input> {
    Ident(&'input str),
    Val(Value<'input>),
    Unary(Unop, Box<Expr<'input>>),
    Assign(&'input str, Box<Expr<'input>>),
    If(Box<Expr<'input>>, Box<Expr<'input>>, Box<Expr<'input>>),
    For(&'input str, Box<Expr<'input>>, Box<Expr<'input>>),
    Block(Vec<Expr<'input>>),
    // Pipe(Box<Expr>, Box<Expr>),
    Binary(Box<Expr<'input>>, &'input BinOp, Box<Expr<'input>>),
    Command(&'input str, Vec<&'input str>, Vec<Expr<'input>>),
    WithRedirection(Box<Expr<'input>>, Redirector),
}

pub const EXPR_PASS: Expr = Expr::Val(Value::VUnit);
