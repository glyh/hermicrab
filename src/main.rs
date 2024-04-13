use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar);

mod ast;
mod parser;
mod parser_helper;

fn main() {
    println!("Hello, world!");
}
