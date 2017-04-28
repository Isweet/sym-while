open Ast
open Symbol

module StringMap = Map.Make(String)

type sym_state = (Symbol.int_t StringMap.t) * Symbol.t

module Impl : (Semantics.T with type answer = sym_state list) =
struct
  type answer = sym_state list

  let string_of_sym_state s_st =
    let (env, pc) = s_st in
    let binds = StringMap.bindings env in
    let env_str = "env = { " ^ (String.concat ", " (List.map (fun (k, v) -> Printf.sprintf "%s -> %s" k (string_of_int_t v)) binds)) ^ " }" in
    let pc_str  = Printf.sprintf "pc = %s" (string_of_t pc) in
    "(" ^ env_str ^ " ; " ^ pc_str ^ ")"

  let string_of_answer s_sts =
    "===\n" ^ (String.concat "" (List.map (fun s_st -> "* " ^ (string_of_sym_state s_st) ^ "\n") s_sts)) ^ "==="

  let rec eval_arith (a : arith) (s_st : sym_state) : Symbol.int_t =
    let (env, _) = s_st in
    match a with
      | AEVar x -> StringMap.find x env
      | AENum n -> LNum n
      | AENegate a' ->
          let int_sym = eval_arith a' s_st in
          (match int_sym with
            | LNum n -> LNum (- n)
            | _      -> int_sym)
      | AEPlus (a1, a2) ->
          let int_sym1 = eval_arith a1 s_st in
          let int_sym2 = eval_arith a2 s_st in
          (match (int_sym1, int_sym2) with
            | (LNum n1, LNum n2) -> LNum (n1 + n2)
            | _                  -> LPlus (int_sym1, int_sym2))
      | AEMinus (a1, a2) ->
          let int_sym1 = eval_arith a1 s_st in
          let int_sym2 = eval_arith a2 s_st in
          (match (int_sym1, int_sym2) with
            | (LNum n1, LNum n2) -> LNum (n1 - n2)
            | _                  -> LMinus (int_sym1, int_sym2))
      | AEMult (a1, a2) ->
          let int_sym1 = eval_arith a1 s_st in
          let int_sym2 = eval_arith a2 s_st in
          (match (int_sym1, int_sym2) with
            | (LNum n1, LNum n2) -> LNum (n1 * n2)
            | _                  -> LMult (int_sym1, int_sym2))
      | AEDiv (a1, a2) ->
          let int_sym1 = eval_arith a1 s_st in
          let int_sym2 = eval_arith a2 s_st in
          (match (int_sym1, int_sym2) with
            | (LNum n1, LNum n2) -> LNum (n1 / n2)
            | _                  -> LDiv (int_sym1, int_sym2))

  let rec eval (s : stmt) (s_st : sym_state) : answer =
    let (env, pc) = s_st in
    match s with
      | SAssign (x, a) ->
          let n = eval_arith a s_st in
          [(StringMap.add x n env, pc)]
      | SInput (x) ->
          [(StringMap.add x (LVar x) env, pc)]
      | SAssert (b) ->
          let sat_true  = check (z3_of_t (t_of_boolean b)) in
          let sat_false = check (z3_of_t (t_of_boolean (BENot b))) in
          let ret = ref [] in
          (match sat_true with
            | Some _ -> ret := (env, LAnd ((t_of_boolean b), pc)) :: !ret
            | None   -> ());
          (match sat_false with
            | Some (ex) -> print_endline (Printf.sprintf "Assertion Violation: %s with model %s" (string_of_boolean b) ex)
            | None      -> ());
          !ret
      | SSkip ->
          [s_st]
      | SSeq (s1, s2) ->
          let s_sts1 = eval s1 s_st in
          List.flatten (List.map (fun s_st1 -> eval s2 s_st1) s_sts1)
      | SIf (b, s1, s2) ->
          let sat_true  = check (z3_of_t (t_of_boolean b)) in
          let sat_false = check (z3_of_t (t_of_boolean (BENot b))) in
          let ret = ref [] in
          (match sat_true with
            | Some _ -> ret := !ret @ (eval s1 (env, LAnd ((t_of_boolean b), pc)))
            | None   -> ());
          (match sat_false with
            | Some _ -> ret := !ret @ (eval s2 (env, LAnd ((t_of_boolean (BENot b)), pc)))
            | None   -> ());
          !ret

  let run (s : stmt) : answer = eval s (StringMap.empty, LTrue)
end
