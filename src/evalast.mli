open Ast

(** Representation of parser model. This module holds the structure of the main
    parser method, formal logic, inputting, processing and representation. *)

exception Incompatible of string
(** Raised when an expression is evaluated with incompatible types *)

val add : expr -> expr -> expr
(** [add e1 e2] computes e2+e1, raises Incompatible if the types do not add*)

val mult : expr -> expr -> expr
(** [mult e1 e2] computes e2*e1, raises Incompatible if the types do not
    multiply*)

val sub : expr -> expr -> expr
(** [sub e1 e2] computes e2-e1, raises Incompatible if the types do not subtract*)

val div : expr -> expr -> expr
(** [div e1 e2] computes e2/e1, raises Incompatible if the types do not divide*)

val len : 'a list -> int
(** [len ls] computes the length of a list*)

val at : expr list -> int -> expr
(** [at ls] finds the expression at specified index of a list of expressions*)

val cross : expr -> expr -> expr
(** [cross e1 e2] typechecks two expressions e1, e2 to vectors and then computes
    their cross product if they meet the right dimension types **)

val and_eval : expr -> expr -> expr
(** [and_eval e1 e2] Evaluates e1 && e2 *)

val or_eval : expr -> expr -> expr
(** [or_eval e1 e2] Evaluates e1 || e2 *)

val eq_eval : expr -> expr -> expr
(** [eq_eval e1 e2] checks that e1 == e2*)

val gt_eval : expr -> expr -> expr
(** [gt_eval e1 e2] checks if one expression e1 is greater than the other e2*)

val parse : string -> expr
(** [parse str] takes an input string and converts it into an expression of type
    expr. *)

val eval : expr -> expr
(** [eval e] takes an expression [e] and evaluates it to some other expression
    expression. *)

val to_string : expr -> string
(** [to_string e] takes an expression [e] and represents it as a string. *)

val interp : string -> string
(** [interp s] interprets a string input. That is, it parses a string into an
    expression, evaluates the expression, and returns the string representation
    of the evaluated expression. *)
