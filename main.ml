open Ast
open Parser

type semantics =
  | Concrete
  | Symbolic

let input      = ref ""

let debug      = ref false
let sem        = ref Concrete
let loop_limit = ref 10

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
  print_endline (SEM.string_of_answer (SEM.run stmt));

  let res = Symbol.check (Symbol.z3_of_t ()) in

  match res with
    | None -> print_endline "ERR\n"
    | Some s -> print_endline s

;;
    
main ()
