   Compiling hmc v0.1.0 (/home/lyh/Documents/CS/hmc/hermicrab)
warning: unused import: `Lexer`
 --> src/parser.rs:1:13
  |
1 | use logos::{Lexer, Logos, SpannedIter};
  |             ^^^^^
  |
  = note: `#[warn(unused_imports)]` on by default

warning: variable does not need to be mutable
    --> /home/lyh/Documents/CS/hmc/hermicrab/target/debug/build/hmc-359d0aa5e70bbb64/out/grammar.rs:9367:9
     |
9367 |     (_, mut v, _): (usize, alloc::vec::Vec<Box<Expr<'input>>>, usize),
     |         ----^
     |         |
     |         help: remove this `mut`
     |
     = note: `#[warn(unused_mut)]` on by default

warning: variable does not need to be mutable
    --> /home/lyh/Documents/CS/hmc/hermicrab/target/debug/build/hmc-359d0aa5e70bbb64/out/grammar.rs:9930:9
     |
9930 |     (_, mut head, _): (usize, &'input str, usize),
     |         ----^^^^
     |         |
     |         help: remove this `mut`

warning: variable does not need to be mutable
     --> /home/lyh/Documents/CS/hmc/hermicrab/target/debug/build/hmc-359d0aa5e70bbb64/out/grammar.rs:10228:9
      |
10228 |     (_, mut args, _): (usize, alloc::vec::Vec<Box<Expr<'input>>>, usize),
      |         ----^^^^
      |         |
      |         help: remove this `mut`

warning: variable does not need to be mutable
     --> /home/lyh/Documents/CS/hmc/hermicrab/target/debug/build/hmc-359d0aa5e70bbb64/out/grammar.rs:10260:9
      |
10260 |     (_, mut args, _): (usize, alloc::vec::Vec<Box<Expr<'input>>>, usize),
      |         ----^^^^
      |         |
      |         help: remove this `mut`

warning: type `Placeholder` is more private than the item `Expr::Unary::0`
  --> src/ast.rs:38:11
   |
38 |     Unary(Unop, Box<Expr<'input>>),
   |           ^^^^ field `Expr::Unary::0` is reachable at visibility `pub(crate)`
   |
note: but type `Placeholder` is only usable at visibility `pub(self)`
  --> src/ast.rs:14:1
   |
14 | struct Placeholder(());
   | ^^^^^^^^^^^^^^^^^^
   = note: `#[warn(private_interfaces)]` on by default

warning: type `Placeholder` is more private than the item `Expr::WithRedirection::1`
  --> src/ast.rs:46:40
   |
46 |     WithRedirection(Box<Expr<'input>>, Redirector),
   |                                        ^^^^^^^^^^ field `Expr::WithRedirection::1` is reachable at visibility `pub(crate)`
   |
note: but type `Placeholder` is only usable at visibility `pub(self)`
  --> src/ast.rs:14:1
   |
14 | struct Placeholder(());
   | ^^^^^^^^^^^^^^^^^^

warning: variants `VInt` and `ProcLam` are never constructed
  --> src/ast.rs:23:5
   |
21 | pub enum Value<'input> {
   |          ----- variants in this enum
22 |     VUnit,
23 |     VInt(i64),
   |     ^^^^
...
26 |     ProcLam(Vec<&'input str>, Box<Expr<'input>>),
   |     ^^^^^^^
   |
   = note: `#[warn(dead_code)]` on by default

warning: variants `Ident`, `Unary`, `Assign`, and `WithRedirection` are never constructed
  --> src/ast.rs:36:5
   |
35 | pub enum Expr<'input> {
   |          ---- variants in this enum
36 |     Ident(&'input str),
   |     ^^^^^
37 |     Val(Value<'input>),
38 |     Unary(Unop, Box<Expr<'input>>),
   |     ^^^^^
39 |     Assign(&'input str, Box<Expr<'input>>),
   |     ^^^^^^
...
46 |     WithRedirection(Box<Expr<'input>>, Redirector),
   |     ^^^^^^^^^^^^^^^

warning: associated function `new` is never used
   --> src/parser.rs:177:12
    |
176 | impl<'input> WrappedLexer<'input> {
    | --------------------------------- associated function in this implementation
177 |     pub fn new(input: &'input str) -> Self {
    |            ^^^

warning: `hmc` (bin "hmc") generated 10 warnings (run `cargo fix --bin "hmc"` to apply 5 suggestions)
    Finished dev [unoptimized + debuginfo] target(s) in 3.09s
