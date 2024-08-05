open Ast

(** This module defines the structure of sets along with its associated
    functions. *)

exception InvalidSet of string
(** Raised when the set is invalid *)

val empty : unit -> expr
(** Initializes empty set *)

val is_empty : expr -> bool
(** [is_empty s] is true if and only if the [s] is empty *)

val is_in : expr -> expr -> expr
(** [is_in x s] true if and only if the [x] is in the [s] *)

val add : expr -> expr -> expr
(** [add x s] adds element [x] to the set [s]. If [x] is already present in [s],
    no changes are made. Returns the updated set. *)

val remove : expr -> expr -> expr
(** [remove x s] removes element [x] from the set [s] if possible, otherwise
    returns the set [s] unchanged. Returns the updated set. *)

val union : expr -> expr -> expr
(** [union s1 s2] returns the union of sets [s1] and [s2]. The union of two sets
    is a set that contains all the distinct elements from both sets. *)

val intersect : expr -> expr -> expr
(** [intersect s1 s2] returns the intersection of sets [s1] and [s2]. The
    intersection of two sets is a set that contains only the elements that are
    common to both sets. *)

val is_subset : expr -> expr -> bool
(** [is_subset s1 s2] returns true if and only if set [s1] is a subset of set
    [s2]. A set [s1] is considered a subset of [s2] if all elements of [s1] are
    also present in [s2]. *)

val equals : expr -> expr -> bool
(** [equals s1 s2] returns true if and only if set [s1] and set [s2] are equal,
    meaning they are subsets of each other. *)
