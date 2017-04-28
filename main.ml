open Ast
open Parser

type semantics =
  | Concrete
  | Symbolic

let input      = ref ""

let debug      = ref false
let sem        = ref Concrete
let loop_limit = ref 10


let string_of_token t =
  match t with
    | EOF -> exit 0
    | VAR x -> Printf.sprintf "VAR %s" x
    | NUM n -> Printf.sprintf "NUM %s" (string_of_int n)
    | PLUS  -> Printf.sprintf "PLUS"
    | MINUS  -> Printf.sprintf "MINUS"
    | MULT  -> Printf.sprintf "MULT"
    | DIV  -> Printf.sprintf "DIV"
    | TRUE  -> Printf.sprintf "TRUE"
    | FALSE  -> Printf.sprintf "FALSE"
    | NOT  -> Printf.sprintf "NOT"
    | AND  -> Printf.sprintf "AND"
    | OR  -> Printf.sprintf "OR"
    | LT  -> Printf.sprintf "LT"
    | GT  -> Printf.sprintf "GT"
    | ASSIGN  -> Printf.sprintf "ASSIGN"
    | SKIP  -> Printf.sprintf "SKIP"
    | SEMI  -> Printf.sprintf "SEMI"
    | LPAREN  -> Printf.sprintf "LPAREN"
    | RPAREN  -> Printf.sprintf "RPAREN"
    | IF  -> Printf.sprintf "IF"
    | THEN  -> Printf.sprintf "THEN"
    | ELSE  -> Printf.sprintf "ELSE"
    | WHILE  -> Printf.sprintf "WHILE"
    | DO  -> Printf.sprintf "DO"
    | DONE -> Printf.sprintf "DONE"
    | INPUT -> Printf.sprintf "INPUT"

let if_debug f =
  if !debug then
    f ()
  else
    ()

let main () = 
  Arg.parse [
    ("--debug",
     Arg.Set debug,
     "Enable debug printing. (default = off)");
    ("--semantics",
     Arg.String (fun s -> match s with
                            | "concrete" -> sem := Concrete
                            | "symbolic" -> sem := Symbolic
                            | _          -> raise (Arg.Bad "--semantics (concrete|symbolic)")),
     "Choose semantics with which to run `while` stmt. (default = \"concrete\")");
    ("--loop-limit",
     Arg.Set_int loop_limit,
     "Choose the maximum number of loop iterations to analyze. (default = 10)")
  ] (function s -> input := s) "usage: sym-while (<input file>|--)";

  let chan = open_in !input in
  let lexbuf = Lexing.from_channel chan in
  let stmt   = Parser.main Lexer.token lexbuf in

  let module SEM =
    (val (match !sem with
            | Concrete -> (module Concrete.Impl : Semantics.T)
            | Symbolic -> (module Symbolic.Impl : Semantics.T)) : Semantics.T) in

  if_debug (fun () -> print_endline (string_of_stmt stmt));
  print_endline (SEM.string_of_answer (SEM.run stmt))

;;
    
main ()
