%{
  open Ast
%}

%token EOF
%token <string> VAR
%token <int> NUM
%token PLUS
%token MINUS
%token MULT
%token DIV
%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token LT
%token GT
%token EQ
%token ASSIGN
%token SKIP
%token SEMI
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token DONE
%token INPUT
%token ASSERT

/* Arithmetic */
%left PLUS MINUS
%left MULT DIV

/* Relational */
%left LT GT EQ

/* Logical */
%left AND OR
%nonassoc NOT

/* Sequencing */
%right DO
%right SEMI
%left ELSE

%type <Ast.stmt> main
%start main

%%
main:
  stmt EOF { $1 }
;
stmt:
  | VAR ASSIGN arith               { SAssign ($1, $3) }
  | INPUT LPAREN VAR RPAREN        { SInput ($3) }
  | ASSERT boolean                 { SAssert ($2) }
  | SKIP                           { SSkip }
  | stmt SEMI stmt                 { SSeq ($1, $3) }
  | IF boolean THEN stmt ELSE stmt { SIf ($2, $4, $6) }
  | WHILE boolean DO stmt DONE     { SWhile ($2, $4) }
;
boolean:
  | TRUE                  { BETrue }
  | FALSE                 { BEFalse }
  | NOT boolean           { BENot ($2) }
  | boolean AND boolean   { BEAnd ($1, $3) }
  | boolean OR  boolean   { BEOr ($1, $3) }
  | arith   LT  arith     { BELT ($1, $3) }
  | arith   GT  arith     { BEGT ($1, $3) }
  | arith   EQ  arith     { BEEq ($1, $3) }
  | LPAREN boolean RPAREN { $2 }
;
arith:
  | VAR                 { AEVar ($1) }
  | NUM                 { AENum ($1) }
  | MINUS arith         { AENegate ($2) }
  | arith PLUS  arith   { AEPlus  ($1, $3) }
  | arith MINUS arith   { AEMinus ($1, $3) }
  | arith MULT  arith   { AEMult  ($1, $3) }
  | arith DIV   arith   { AEDiv   ($1, $3) }
  | LPAREN arith RPAREN { $2 }
;
