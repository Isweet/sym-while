open Ast

module type T =
sig
  type answer

  val string_of_answer: answer -> string

  val run: stmt -> answer
end

  
