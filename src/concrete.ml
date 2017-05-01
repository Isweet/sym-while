open Ast

module StringMap = Map.Make(String)

type conc_state = int StringMap.t

let string_of_conc_state c_st =
  let binds = StringMap.bindings c_st in
  "< env = { " ^ (String.concat ", " (List.map (fun (k, v) -> Printf.sprintf "%s -> %s" k (string_of_int v)) binds)) ^ " } >"

module Impl : (Semantics.T with type answer = conc_state) =
struct
  type answer = conc_state

  let string_of_answer = string_of_conc_state

  let rec eval_arith (a : arith) (c_st : conc_state) : int =
    match a with
      | AEVar x -> StringMap.find x c_st
      | AENum n -> n
      | AENegate a' -> - (eval_arith a' c_st)
      | AEPlus (a1, a2) -> (eval_arith a1 c_st) + (eval_arith a2 c_st)
      | AEMinus (a1, a2) -> (eval_arith a1 c_st) - (eval_arith a2 c_st)
      | AEMult (a1, a2) -> (eval_arith a1 c_st) * (eval_arith a2 c_st)
      | AEDiv (a1, a2) -> (eval_arith a1 c_st) / (eval_arith a2 c_st)

  let rec eval_bool (b : boolean) (c_st : conc_state) : bool =
    match b with
      | BETrue -> true
      | BEFalse -> false
      | BENot b' -> not (eval_bool b' c_st)
      | BEAnd (b1, b2) -> (eval_bool b1 c_st) && (eval_bool b2 c_st)
      | BEOr (b1, b2) -> (eval_bool b1 c_st) || (eval_bool b2 c_st)
      | BELT (a1, a2) -> (eval_arith a1 c_st) < (eval_arith a2 c_st)
      | BEGT (a1, a2) -> (eval_arith a1 c_st) > (eval_arith a2 c_st)
      | BEEq (a1, a2) -> (eval_arith a1 c_st) = (eval_arith a2 c_st)

  let rec eval (s : stmt) (c_st : conc_state) : answer =
    match s with
      | SAssign (x, a) ->
          let n = eval_arith a c_st in
          StringMap.add x n c_st
      | SInput (x) ->
          let n = int_of_string (read_line ()) in
          StringMap.add x n c_st
      | SAssert (b) ->
          if eval_bool b c_st then
            c_st
          else
            (print_endline (Printf.sprintf "Assertion Failed: %s with\n %s" (string_of_boolean b) (string_of_conc_state c_st));
            exit 1)
      | SSkip ->
          c_st
      | SSeq (s1, s2) ->
          let st'  = eval s1 c_st in
          let st'' = eval s2 st'  in
          st''
      | SIf (b, s1, s2) ->
          let guard = eval_bool b c_st in
          if guard then eval s1 c_st else eval s2 c_st
      | SWhile (b, body) ->
          let guard = eval_bool b c_st in
          if guard then
            let st' = eval body c_st in
            eval s st'
          else
            c_st

  let run s = eval s StringMap.empty


end
