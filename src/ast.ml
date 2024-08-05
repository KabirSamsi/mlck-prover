(** Abstract Syntax Tree type *)

(* Unary operators *)
type unop =
  | Not
  | Inv

(* Binary operators *)
type binop =
  | Add
  | Sub
  | Mult
  | Div
  | And
  | Or
  | Xor
  | Eq
  | Neq
  | Gt
  | Lt
  | Geq
  | Leq
  | Cross
  | Setadd
  | Setremove
  | Union
  | Intersection
  | Subset
  | Setequals
  | Implies

type quantifier =
  | All
  | Exists

type var =
  | Free of string
  | Entity of string
  | Function of string * var list

(* Expressions *)
type expr =
  | None
  | Boolean of bool
  | Integer of int
  | Float of float
  | Str of string
  | Character of char
  | Vector of expr list
  | Set of expr list
  | UnExpr of unop * expr
  | BopExpr of binop * expr * expr
  | PredExpr of string * var list
  | QuantExpr of quantifier * var * expr
