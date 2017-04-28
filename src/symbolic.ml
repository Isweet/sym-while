open Ast

module StringMap = Map.Make(String)

type sym_state = Symbol.t StringMap.t

module Impl : (Semantics.T with type answer = sym_state list) =
struct
  type answer = sym_state list

  let string_of_answer s_sts = ""

  let run (s : stmt) : answer = []
end
