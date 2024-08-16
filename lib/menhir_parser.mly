%{
  [@@@coverage exclude_file]
  open Ast

  let pass = Val (Unit)
  (* TODO: this should be of exitcode type *)
  (*let fail = Val (Int 1)*)

  let make_binary (op: string) lhs rhs =
    Command(op, [], [lhs; rhs])

  let make_unary op inner =
    Command(op, [], [inner])

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
%token MULTISTR_INTERPOL

%token <string * string> SIGIL
%token <string * string * string> SIGIL_REGEX_LIKE

%token <string> WORD
%token UNIT

%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE

%token FSLASH
%token IF WHEN ELSE FOR DO IN AND_THEN OR_ELSE
%token TRUE FALSE
%token PROC
%token ASSIGN DECLARE
%token PIPE COMMA COLON SEMICOLON
%token PLUS MINUS MULT DIV MOD
%token EQ NE LE GE LT GT
%token AND OR NOT XOR
%token BSHIFTL BSHIFTR
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
  | e1=ThisTier op=Op e2=NextTier { 
    make_binary op e1 e2
  }
  | e=NextTier { e }

tiersLeftF(ThisTier, OpF, NextTier):
  | e1=ThisTier opf=OpF e2=NextTier { 
    opf e1 e2
  }
  | e=NextTier { e }

op_orelse:
  | OR_ELSE { fun lhs rhs -> OrElse(lhs, rhs) }
op_andthen:
  | AND_THEN { fun lhs rhs -> AndThen(lhs, rhs) }
op_pipe:
  | PIPE { fun lhs rhs -> Pipe(lhs, rhs) }

command_exp_requires_term:
  | e=tiersLeftF(command_exp_requires_term, op_orelse, command_exp_requires_term1) { e }
command_exp_requires_term1:
  | e=tiersLeftF(command_exp_requires_term1, op_andthen, command_exp_requires_term2) { e }
command_exp_requires_term2:
  | e=tiersLeftF(command_exp_requires_term2, op_pipe, command_exp_requires_term3) { e }
command_exp_requires_term3:
  | c=command_call_no_nl { c }

command_exp_requires_no_term:
  | ASSIGN e=expression { e }
  | e=tiersLeftF(command_exp_requires_no_term, op_orelse, command_exp_requires_no_term1) { e }
command_exp_requires_no_term1:
  | e=tiersLeftF(command_exp_requires_no_term1, op_andthen, command_exp_requires_no_term2) { e }
command_exp_requires_no_term2:
  | e=tiersLeftF(command_exp_requires_no_term2, op_pipe, command_exp_requires_no_term3) { e }

command_exp_requires_no_term3:
  | LPAREN NL* e=expression NL* RPAREN { e }
  | DO b=block { b }

word_arg:
  | w=WORD { Just w }
  | dec=DECIMAL_LIT { Just dec }
  | hex=HEX_LIT { Just hex }
  | str=LIT_STR { Just str }
  | TRUE { Just "true" } 
  | FALSE { Just "false" } 
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
  | UNIT { [] }

typed_args_no_nl:
  | args=typed_args_no_blk blk=option(block) {
    match blk with
    | Some(blk) -> args @ [blk]
    | None -> args
  }

op_xor:
  | XOR { "xor" }
expression:
  | e=tiersLeft(expression, op_xor, expr1)  { e }

op_or:
  | OR { "or" }
expr1:
  | e=tiersLeft(expr1, op_or, expr2)  { e }

op_and:
  | AND { "and" }
expr2:
  | e=tiersLeft(expr2, op_and, expr3)  { e }

op_rel:
  | EQ { "==" }
  | NE { "!=" }
  | LE { "<=" }
  | GE { ">=" }
  | LT { "<" }
  | GT { ">" }
expr3:
  | e=tiersLeft(expr3, op_rel, expr6)  { e }

op_bshift:
  | BSHIFTL { "shl" }
  | BSHIFTR { "shr" }

expr6:
  | e=tiersLeft(expr6, op_bshift, expr7)  { e }

op_concat:
    | CONCAT { "++" }
expr7:
  | e=tiersLeft(expr7, op_concat, expr8)  { e }

op_add:
  | PLUS { "+" }
  | MINUS { "-" }
expr8:
  | e=tiersLeft(expr8, op_add, expr9) { e }

op_mul:
  | MULT { "*" }
  | DIV { "/" } 
  | MOD { "mod" }
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
  | NOT { "not" }

expr_unary:
  | op=unop inner=expr_unary { make_unary op inner }
  | e=expr_primary { e }

expr_primary: 
  | LPAREN NL* e=expression NL* RPAREN { e }
  | b=block { b }
  | str=templ_str { str }
  | str=multi_str { str }
  | l=literal { Val(l) }
  | lam=proc_lam { Val lam }
  | id=WORD { Var(id) }
  (* TODO: SIGILS *)

params_trail:
  | p=WORD NL* option(comma_nl) {
        [p]
  }
  | p=WORD NL* COMMA NL* ps=params_trail {
    p :: ps
  }

params:
  | LPAREN NL* p=params_trail RPAREN { p }
  | LPAREN NL* RPAREN { [] }
  | UNIT { [] }

proc_lam:
  | FSLASH ws=list(WORD) ts=params LBRACE NL* cs=blockRestWithTerm(RBRACE) {
    ProcLam(ws, ts, MBlock(cs))
  }

templ_str:
  | s=TEMPL_STR { Val(Str s) }
  | s=TEMPL_STR_R s_rest=templ_str_rest { make_binary "++" (Val(Str s)) s_rest }

templ_str_rest:
  | NL* e=expression NL* s_last=TEMPL_STR_L
    {
      let stringified = make_unary "stringify" e in
      make_binary "++" stringified (Val(Str s_last))
    }
  | NL* e=expression NL* s_middle=TEMPL_STR_LR s_rest=templ_str_rest
    {
      let stringified = make_unary "stringify" e in
      make_binary "++" (make_binary "++" stringified (Val(Str s_middle))) s_rest
    }

multi_str_part:
    | s=MULTISTR_PART {
      Val(Str s)
    }
    | MULTISTR_INTERPOL e=expression RBRACE NL {
      e
    } 

multi_str:
    | s_head=multi_str_part ss=list(multi_str_part) {
      List.fold_left (fun acc s -> make_binary "++" acc s) s_head ss
    }

literal:
  | UNIT { Unit } 
  | TRUE { Bool true } 
  | FALSE { Bool false } 
  | i=DECIMAL_LIT { Int(int_of_string(i)) }
  | i=HEX_LIT { Int(int_of_string(i)) }
  | i=OCT_LIT { Int(int_of_string(i)) }
  | i=BIN_LIT { Int(int_of_string(i)) }
  | str=LIT_STR { Str str }
