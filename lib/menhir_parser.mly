%{
  [@@@coverage exclude_file]
  open Ast

  let pass = Val (Unit)
  (* TODO: this should be of exitcode type *)
  (*let fail = Val (Int 1)*)

%}

%token NL
%token EOF

%token <string> DECIMAL_LIT
%token <string> HEX_LIT
%token <string> OCT_LIT
%token <string> BIN_LIT

%token <string> LIT_STR

(*
  Having R means the templated string is being open at its right end,
  while having L means the templated string is being open at its left end.
*)
%token <string> TEMPL_STR
%token <string> TEMPL_STR_R
%token <string> TEMPL_STR_L
%token <string> TEMPL_STR_LR

%token <string> MULTISTR_PART
%token <string> MULTISTR_PART_R
%token <string> MULTISTR_PART_L
%token <string> MULTISTR_PART_LR

%token <string * string> SIGIL
%token <string * string * string> SIGIL_REGEX_LIKE

%token <string> WORD
%token UNIT

%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE

%token IF WHEN ELSE FOR DO IN AND_THEN OR_ELSE
%token ASSIGN DECLARE
%token PIPE COMMA COLON SEMICOLON
%token PLUS MINUS MULT DIV REM
%token EQ NE LE GE LT GT
%token AND OR NOT
%token BSHIFTL BSHIFTR BXOR BAND BOR
%token CONCAT

%start <expr> program_eof

%%

program_eof: 
  | NL* cs=blockRestWithTerm(EOF) { 
    match cs with
    | [] -> pass
    | b -> MBlock(b)
  }

command:
  | e=exp_single {e}
  | IF NL* test=command NL* then_clause=orBlk(command) NL* ELSE NL* else_clause=orBlk(command) {
    If(test, then_clause, else_clause)
  }
  (* NOTE:
     For n dangling constructs I have to have 2^n rules!
     Hence we differentiate with `when`  
  *)
  | WHEN NL* test=command NL* then_clause=orBlk(command) {
    When(test, then_clause)
  }
  | FOR NL* sentinal=WORD NL* IN NL* collection=command NL* body=orBlk(command) {
      For(sentinal, collection, body)
  }

orBlk(Rule):
  | r=Rule { r }
  | b=block { b }

exp_single:
  | c=command_exp_requires_term NL { c }
  | c=command_exp_requires_term SEMICOLON { c }
  | c=command_exp_requires_no_term { c }
  | lhs=WORD ASSIGN rhs=expression { Assign(lhs, rhs, false) }
  | lhs=WORD DECLARE rhs=expression { Assign(lhs, rhs, true) }

tiersLeft(ThisTier, Op, NextTier):
  | e1=ThisTier op=Op e2=NextTier { Binary(op, e1, e2) }
  | e=NextTier { e }

op_orelse:
  | OR_ELSE { OrElse }
op_andthen:
  | AND_THEN { AndThen }
op_pipe:
  | PIPE { Pipe }

command_exp_requires_term:
  | e=tiersLeft(command_exp_requires_term, op_orelse, command_exp_requires_term1) { e }
command_exp_requires_term1:
  | e=tiersLeft(command_exp_requires_term1, op_andthen, command_exp_requires_term2) { e }
command_exp_requires_term2:
  | e=tiersLeft(command_exp_requires_term2, op_pipe, command_exp_requires_term3) { e }
command_exp_requires_term3:
  | c=command_call_no_nl { c }

command_exp_requires_no_term:
  | ASSIGN e=expression { e }
  | e=tiersLeft(command_exp_requires_no_term, op_orelse, command_exp_requires_no_term1) { e }
command_exp_requires_no_term1:
  | e=tiersLeft(command_exp_requires_no_term1, op_andthen, command_exp_requires_no_term2) { e }
command_exp_requires_no_term2:
  | e=tiersLeft(command_exp_requires_no_term2, op_bor, command_exp_requires_no_term3) { e }

command_exp_requires_no_term3:
  | LPAREN NL* e=expression NL* RPAREN { e }
  | DO b=block { b }

word_arg:
  | w=WORD { Just w }
  | dec=DECIMAL_LIT { Just dec }
  | hex=HEX_LIT { Just hex }
  | str=LIT_STR { Just str }
  | LBRACKET e=expression RBRACKET { Exp e }

command_call_no_nl:
  | name=WORD args=list(word_arg) targets=option(typed_args_no_nl) {
    let targets = 
      match targets with
      | None -> []
      | Some(ts) -> ts
    in
    Command(name, args, targets)
  }

comma_nl: 
  | COMMA NL* {}

args_trail:
  | arg=expression NL* option(comma_nl) {
        [arg]
  }
  | arg=expression NL* COMMA NL* args=args_trail {
    arg :: args
  }

typed_args_no_blk:
  | LPAREN NL* args=args_trail RPAREN { args }
  | LPAREN NL* RPAREN { [] }

typed_args_no_nl:
  | args=typed_args_no_blk blk=option(block) {
    match blk with
    | Some(blk) -> args @ [blk]
    | None -> args
  }

op_or:
  | OR { LOr }
expression:
  | e=tiersLeft(expression, op_or, expr1)  { e }

op_and:
  | AND { LAnd }
expr1:
  | e=tiersLeft(expr1, op_and, expr2)  { e }

op_rel:
  | EQ { Eq }
  | NE { Ne }
  | LE { Le }
  | GE { Ge }
  | LT { Lt }
  | GT { Gt }
expr2:
  | e=tiersLeft(expr2, op_rel, expr3)  { e }

op_bor:
  | BOR { BOr }
expr3:
  | e=tiersLeft(expr3, op_bor, expr4)  { e }

op_bxor:
  | BXOR { BXor }
expr4:
  | e=tiersLeft(expr4, op_bxor, expr5)  { e }

op_band:
  | BAND { BAnd }
expr5:
  | e=tiersLeft(expr5, op_band, expr6)  { e }

op_bshift:
  | BSHIFTL { BShiftL }
  | BSHIFTR { BShiftR }

expr6:
  | e=tiersLeft(expr6, op_bshift, expr7)  { e }

op_concat:
    | CONCAT { SConcat }
expr7:
  | e=tiersLeft(expr7, op_concat, expr8)  { e }

op_add:
  | PLUS { Add }
  | MINUS { Sub }
expr8:
  | e=tiersLeft(expr8, op_add, expr9) { e }

op_mul:
  | MULT { Mul }
  | DIV { Div } 
  | REM { Rem }
expr9:
  | e=tiersLeft(expr9, op_mul, expr_unary) { e }

blockRestWithTerm(Term):
  | Term { [] }
  | cmd=command_exp_requires_term Term {
    [cmd]
  }
  | c=command NL* cs=blockRestWithTerm(Term) { 
    c :: cs
  }

block:
  | LBRACE NL* rest=blockRestWithTerm(RBRACE) { 
    match rest with
    | [] -> pass
    | b -> Block(b)
  }

unop:
  | NOT { Not }

expr_unary:
  | op=unop inner=expr_unary { Unary(op, inner) }
  | e=expr_primary { e }

expr_primary: 
  | LPAREN e=expression RPAREN { e }
  | b=block { b }
  | l=literal { Val(l) }

literal:
    | UNIT { Unit } 
    | i=DECIMAL_LIT { Int(int_of_string(i)) }
    | str=LIT_STR { Str str }
    (* TODO: based integers, MULTISTR_PART, TEMPLATE_STR, SIGILS *)

