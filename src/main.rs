use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar);

mod ast;
mod interpreter;
mod parser;
mod parser_helper;

fn main() {
    println!("Hello, world!");
}
