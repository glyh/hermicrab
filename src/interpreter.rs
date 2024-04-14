use crate::ast::{Expr, Value};
use nix::unistd::execv;
use std::{
    collections::HashMap,
    ffi::{CStr, CString},
};

struct Environment<'a> {
    outer: Option<&'a mut Environment<'a>>,
    locals: HashMap<&'a str, Value<'a>>,
}

impl<'a> Environment<'a> {
    fn query(&self, name: &'a str) -> Option<Value<'a>> {
        if let Some(val) = self.locals.get(name) {
            // TODO: figure out how to properly do mem mangement
            Some(val.clone())
        } else if let Some(outer) = &self.outer {
            outer.query(name)
        } else {
            None
        }
    }
}
pub fn evaluate<'a>(expr: Expr<'a>, env: &'a mut Environment<'a>) -> Option<Value<'a>> {
    match expr {
        Expr::Ident(name) => env.query(name),
        Expr::Val(v) => Some(v),
        // Expr::Unary(_, _) => todo!(),
        // Expr::If(_, _, _) => todo!(),
        // Expr::For(_, _, _) => todo!(),
        Expr::Block(exps) => {
            let mut env_inner = Environment {
                outer: Some(env),
                locals: HashMap::new(),
            };
            let mut ret: Option<Value> = None;
            for exp in exps {
                ret = evaluate(exp, &mut env_inner)
            }
            match ret {
                Some(val) => Some(val.to_owned()),
                None => None,
            }
        }
        // Expr::Binary(_, _, _) => todo!(),
        Expr::Command(name, args, typed_args) => {
            if let Some(var) = env.query(name) {
                // procs in scope
                todo!()
            } else {
                // No typed args for external binaries
                assert!(typed_args == vec![]);
                let str_to_cstr = |s: &str| CString::new(s).unwrap();
                let string_to_cstr = |s: String| CString::new(s.as_str()).unwrap();
                let cmd: &CStr = &str_to_cstr(name);
                let args: Vec<CString> = args.clone().into_iter().map(string_to_cstr).collect();
                let _ = execv(cmd, args.as_slice());
                Some(Value::VUnit)
            }
        }
        _ => todo!(),
    }
}
