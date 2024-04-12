struct Placeholder(());

use Placeholder as Binop;
use Placeholder as Redirector;
use Placeholder as Unop;

pub enum Value {
    VUnit,
    VInt(i64),
    VStr(String),
    VIdent(String),
    ProcLam(Vec<String>, Box<Expr>),
}

pub enum Expr {
    Val(Value),
    Unary(Unop, Box<Expr>),
    Binary(Box<Expr>, Binop, Box<Expr>),
    Assign(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    For(String, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Pipe(Box<Expr>, Box<Expr>),
    Command(String, Vec<String>, Vec<Expr>),
    WithRedirection(Box<Expr>, Redirector),
}

pub const EXPR_PASS: Expr = Expr::Val(Value::VUnit);
