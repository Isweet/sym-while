open Ast

type int_t =
  | LVar of string
  | LNum of int
  | LNegate of int_t
  | LPlus of int_t * int_t
  | LMinus of int_t * int_t
  | LMult of int_t * int_t
  | LDiv of int_t * int_t

type t =
  | LTrue
  | LFalse
  | LNot of t
  | LAnd of t * t
  | LOr of t * t
  | LLT of int_t * int_t
  | LGT of int_t * int_t

let config = [("model", "true")]

let context = Z3.mk_context config

let rec int_t_of_arith (a : arith) : int_t =
  match a with
    | AEVar x -> LVar x
    | AENum n -> LNum n
    | AENegate a' -> LNegate (int_t_of_arith a')
    | AEPlus (a1, a2) -> LPlus ((int_t_of_arith a1), (int_t_of_arith a2))
    | AEMinus (a1, a2) -> LMinus ((int_t_of_arith a1), (int_t_of_arith a2))
    | AEMult (a1, a2) -> LMult ((int_t_of_arith a1), (int_t_of_arith a2))
    | AEDiv (a1, a2) -> LDiv ((int_t_of_arith a1), (int_t_of_arith a2))

let rec t_of_boolean (b : boolean) : t =
  match b with
    | BETrue -> LTrue
    | BEFalse -> LFalse
    | BENot b' -> LNot (t_of_boolean b')
    | BEAnd (b1, b2) -> LAnd ((t_of_boolean b1), (t_of_boolean b2))
    | BEOr (b1, b2) -> LOr ((t_of_boolean b1), (t_of_boolean b2))
    | BELT (a1, a2) -> LLT ((int_t_of_arith a1), (int_t_of_arith a2))
    | BEGT (a1, a2) -> LGT ((int_t_of_arith a1), (int_t_of_arith a2))

let rec z3_of_int_t (int_sym : int_t) : Z3.Expr.expr = 
  match int_sym with
    | LVar x -> Z3.Arithmetic.Integer.mk_const_s context x
    | LNum n -> Z3.Arithmetic.Integer.mk_numeral_i context n
    | LNegate int_sym' -> Z3.Arithmetic.mk_unary_minus context (z3_of_int_t int_sym')
    | LPlus (int_sym1, int_sym2) -> Z3.Arithmetic.mk_add context [(z3_of_int_t int_sym1); (z3_of_int_t int_sym2)]
    | LMinus (int_sym1, int_sym2) -> Z3.Arithmetic.mk_sub context [(z3_of_int_t int_sym1); (z3_of_int_t int_sym2)]
    | LMult (int_sym1, int_sym2) -> Z3.Arithmetic.mk_mul context [(z3_of_int_t int_sym1); (z3_of_int_t int_sym2)]
    | LDiv (int_sym1, int_sym2) -> Z3.Arithmetic.mk_div context (z3_of_int_t int_sym1) (z3_of_int_t int_sym2)

let rec z3_of_t (sym : t) : Z3.Expr.expr = 
  match sym with
    | LTrue -> Z3.Boolean.mk_true context
    | LFalse -> Z3.Boolean.mk_false context
    | LNot sym' -> Z3.Boolean.mk_not context (z3_of_t sym')
    | LAnd (sym1, sym2) -> Z3.Boolean.mk_and context [(z3_of_t sym1); (z3_of_t sym2)]
    | LOr (sym1, sym2) -> Z3.Boolean.mk_or context [(z3_of_t sym1); (z3_of_t sym2)]
    | LLT (int_sym1, int_sym2) -> Z3.Arithmetic.mk_lt context (z3_of_int_t int_sym1) (z3_of_int_t int_sym2)
    | LGT (int_sym1, int_sym2) -> Z3.Arithmetic.mk_gt context (z3_of_int_t int_sym1) (z3_of_int_t int_sym2)

let check (ast : Z3.Expr.expr) : string option =
  let solver = Z3.Solver.mk_simple_solver context in
  Z3.Solver.add solver [ ast ];
  let result = Z3.Solver.check solver [] in
  if result = Z3.Solver.SATISFIABLE then
    match Z3.Solver.get_model solver with
      | None   -> failwith "Impossible"
      | Some m -> Some (Z3.Model.to_string m)
  else
    None
