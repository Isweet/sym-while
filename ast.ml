type arith =
  | AEVar of string
  | AENum of int
  | AENegate of arith
  | AEPlus of arith * arith
  | AEMinus of arith * arith
  | AEMult of arith * arith
  | AEDiv of arith * arith

let rec string_of_arith a =
  match a with
    | AEVar x          -> Printf.sprintf "%s" x
    | AENum n          -> Printf.sprintf "%s" (string_of_int n)
    | AENegate a       -> Printf.sprintf "-%s" (string_of_arith a)
    | AEPlus (a1, a2)  -> Printf.sprintf "(%s + %s)" (string_of_arith a1) (string_of_arith a2)
    | AEMinus (a1, a2) -> Printf.sprintf "(%s - %s)" (string_of_arith a1) (string_of_arith a2)
    | AEMult (a1, a2)  -> Printf.sprintf "(%s * %s)" (string_of_arith a1) (string_of_arith a2)
    | AEDiv (a1, a2)   -> Printf.sprintf "(%s / %s)" (string_of_arith a1) (string_of_arith a2)

type boolean =
  | BETrue
  | BEFalse
  | BENot of boolean
  | BEAnd of boolean * boolean
  | BEOr of boolean * boolean
  | BELT of arith * arith
  | BEGT of arith * arith

let rec string_of_boolean b =
  match b with
    | BETrue  -> Printf.sprintf "true"
    | BEFalse -> Printf.sprintf "false"
    | BENot b -> Printf.sprintf "not %s" (string_of_boolean b)
    | BEAnd (b1, b2) -> Printf.sprintf "(%s and %s)" (string_of_boolean b1) (string_of_boolean b2)
    | BEOr  (b1, b2) -> Printf.sprintf "(%s or %s)" (string_of_boolean b1) (string_of_boolean b2)
    | BELT  (a1, a2) -> Printf.sprintf "(%s < %s)" (string_of_arith a1) (string_of_arith a2)
    | BEGT  (a1, a2) -> Printf.sprintf "(%s > %s)" (string_of_arith a1) (string_of_arith a2)

type stmt = 
  | SAssign of string * arith
  | SInput of string
  | SSkip
  | SSeq of stmt * stmt
  | SIf of boolean * stmt * stmt
  | SWhile of boolean * stmt

let rec string_of_stmt s =
  match s with
    | SAssign (x, a)  -> Printf.sprintf "%s := %s" x (string_of_arith a)
    | SInput x        -> Printf.sprintf "input(%s)" x
    | SSkip           -> Printf.sprintf "skip"
    | SSeq (s1, s2)   -> Printf.sprintf "%s; %s" (string_of_stmt s1) (string_of_stmt s2)
    | SIf (b, s1, s2) -> Printf.sprintf "if %s then %s else %s" (string_of_boolean b) (string_of_stmt s1) (string_of_stmt s2)
    | SWhile (b, s)   -> Printf.sprintf "while %s do %s" (string_of_boolean b) (string_of_stmt s)
