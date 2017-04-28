{
  open Lexing
  open Parser
}

let whitespace = [' ' '\t' '\n' '\r']+

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let digit = ['0'-'9']
let num   = digit digit*

let plus = '+'
let minus = '-'
let mult = '*'
let div  = '/'

let k_true  = "true"
let k_false = "false"

let k_not  = "not"

let k_and  = "and"
let k_or   = "or"

let lt   = '<'
let gt   = '>'
let eq   = '='

let assign = ":="

let k_skip = "skip"

let semi = ';'

let lparen = '('
let rparen = ')'

let k_if   = "if"
let k_then = "then"
let k_else = "else"

let k_while = "while"
let k_do    = "do"
let k_done  = "done"

let k_input = "input"

let k_assert = "assert"

rule token = parse
    whitespace  { token lexbuf }
  | plus        { PLUS }
  | minus       { MINUS }
  | mult        { MULT }
  | div         { DIV }
  | k_true      { TRUE }
  | k_false     { FALSE }
  | k_not       { NOT }
  | k_and       { AND }
  | k_or        { OR }
  | assign      { ASSIGN }
  | lt          { LT }
  | gt          { GT }
  | eq          { EQ }
  | k_skip      { SKIP }
  | semi        { SEMI }
  | lparen      { LPAREN }
  | rparen      { RPAREN }
  | k_if        { IF }
  | k_then      { THEN }
  | k_else      { ELSE }
  | k_while     { WHILE }
  | k_done      { DONE }
  | k_do        { DO }
  | k_input     { INPUT }
  | k_assert    { ASSERT }
  | ident       { VAR (Lexing.lexeme lexbuf) }
  | num         { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | eof         { EOF } 
  | _ as bad    { Printf.printf "Illegal character %c" bad; failwith "Error: Lexer" }
  
