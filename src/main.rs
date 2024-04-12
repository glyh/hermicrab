use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar);

mod ast;
mod parser;

fn main() {
    println!("Hello, world!");
}
