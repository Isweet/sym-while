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

  let rec int_t_of_arith (a : arith) (env : Symbol.int_t StringMap.t) : int_t =
    match a with
      | AEVar x -> StringMap.find x env
      | AENum n -> LNum n
      | AENegate a' -> LNegate (int_t_of_arith a' env)
      | AEPlus (a1, a2) -> LPlus ((int_t_of_arith a1 env), (int_t_of_arith a2 env))
      | AEMinus (a1, a2) -> LMinus ((int_t_of_arith a1 env), (int_t_of_arith a2 env))
      | AEMult (a1, a2) -> LMult ((int_t_of_arith a1 env), (int_t_of_arith a2 env))
      | AEDiv (a1, a2) -> LDiv ((int_t_of_arith a1 env), (int_t_of_arith a2 env))

  let rec t_of_boolean (b : boolean) (env : Symbol.int_t StringMap.t) : Symbol.t =
    match b with
      | BETrue -> LTrue
      | BEFalse -> LFalse
      | BENot b' -> LNot (t_of_boolean b' env)
      | BEAnd (b1, b2) -> LAnd ((t_of_boolean b1 env), (t_of_boolean b2 env))
      | BEOr (b1, b2) -> LOr ((t_of_boolean b1 env), (t_of_boolean b2 env))
      | BELT (a1, a2) -> LLT ((int_t_of_arith a1 env), (int_t_of_arith a2 env))
      | BEGT (a1, a2) -> LGT ((int_t_of_arith a1 env), (int_t_of_arith a2 env))
      | BEEq (a1, a2) -> LEq ((int_t_of_arith a1 env), (int_t_of_arith a2 env))

  let rec eval (s : stmt) (s_st : sym_state) : answer =
    let (env, pc) = s_st in
    match s with
      | SAssign (x, a) ->
          let n = int_t_of_arith a env in
          [(StringMap.add x n env, pc)]
      | SInput (x) ->
          [(StringMap.add x (LVar x) env, pc)]
      | SAssert (b) ->
          let l = t_of_boolean b env in
          let cond_true = LAnd (l, pc) in
          let cond_false = LAnd ((LNot l), pc) in

          let sat_true = check (z3_of_t cond_true) in
          let sat_false = check (z3_of_t cond_false) in

          let ret = ref [] in
          (match sat_true with
            | Some _ -> ret := (env, cond_true) :: !ret
            | None   -> ());
          (match sat_false with
            | Some (ex) -> print_endline (Printf.sprintf "Assertion Violation: %s with model...\n %s" (string_of_boolean b) ex)
            | None      -> ());
          !ret
      | SSkip ->
          [s_st]
      | SSeq (s1, s2) ->
          let s_sts1 = eval s1 s_st in
          List.flatten (List.map (fun s_st1 -> eval s2 s_st1) s_sts1)
      | SIf (b, s1, s2) ->
          let l = t_of_boolean b env in
          let cond_true = LAnd (l, pc) in
          let cond_false = LAnd ((LNot l), pc) in

          let sat_true = check (z3_of_t cond_true) in
          let sat_false = check (z3_of_t cond_false) in

          let ret = ref [] in
          (match sat_true with
            | Some _ -> ret := (eval s1 (env, cond_true)) @ !ret
            | None   -> ());
          (match sat_false with
            | Some _ -> ret := (eval s2 (env, cond_false)) @ !ret
            | None   -> ());
          !ret
      | SWhile (b, body) ->
          let l = t_of_boolean b env in
          let cond_true = LAnd (l, pc) in
          let cond_false = LAnd ((LNot l), pc) in

          let sat_true = check (z3_of_t cond_true) in
          let sat_false = check (z3_of_t cond_false) in

          let ret = ref [] in
          (match sat_true with
            | Some _ -> 
                let s_sts' = eval body (env, cond_true) in
                ret := List.flatten (List.map (fun s_st' -> eval s s_st') s_sts') @ !ret
            | None -> ());
          (match sat_false with
            | Some _ -> ret := (env, cond_false) :: !ret
            | None -> ());
          !ret

  let run (s : stmt) : answer = eval s (StringMap.empty, LTrue)
end
