(** Module for defining first-order predicate logic expressions and a
    resolution-based automated theorem prover.

    Conversion to CNF: https://en.wikipedia.org/wiki/Conjunctive_normal_form
    Reference for theorem proving using resolution :
    https://ocw.mit.edu/courses/6-825-techniques-in-artificial-intelligence-sma-5504-fall-2002/17e67a940531979f0e93adb8a5281bf1_Lecture8FinalPart1.pdf *)

type var
type quantifier
type expr
type binop
type unop

(** TODO:

    - expression application using substitution
    - verbosity toggle (how much to print in the proof) *)

val toggle_verbosity : unit -> unit
(* [toggle_verbosity ()] sets verbosity on/off. If true, functions will print to
   standard output. By default, verbosity is true. *)

val to_string : expr -> string
(** [to_string expr] converts [expr] to string form for pretty printing *)

val is_well_formed : expr -> bool
(** [is_well_formed expr] checks if [expr] is a well-formed expression under
    first-order logic rules: 1) all variables are bounded 2) all quantifiers
    operate on single free variables *)

val prove : expr list -> expr -> bool
(** [prove givens expr] attempts to prove expr from [givens] using resolution
    (assumes that [expr] is false and tries to arrive at a contradiction with
    the expressions in [givens]). Returns true if [expr] is entailed by [givens]
    and false if it cannot be proven. Requires: all the expressions in [givens]
    and [expr] are well-formed *)

val equivalent : expr -> expr -> bool
(** [equivalent expr1 expr2] is true and only if expr1 <=> expr2. Requires:
    [expr1] and [expr2] are well-formed *)

val variables : expr -> var list
(** [variables expr] is the list of all free variables, entities, and
    non-predicate function values in [expr] *)

val unbound_variables : expr -> var list
(** [unbound_variables expr] returns all the unbound variables in [expr]. A
    variable is bound if it is an entity or is quantified over. *)

val substitute_expr : expr -> var -> var -> expr
(** [substitute_expr expr x y] replaces all instances of variable y with x in
    expr. *)

val unify_exprs : expr list -> (var * var) list option
(** [unify_list exprs] finds a list of substitution pairs
    [(x_1, y_1); ...; (x_n, y_n)] that unifies all expressions in [exprs], if
    one exists. Requires: all expressions in [expr] are in Clausal Form. *)

val clausal_form : expr -> expr list
(** [clausal_form expr] converts [expr] into a list of expressions in implicit
    conjunctive normal form without apparent quantifiers (only implicit
    universal quantifiers) so resolution-based proving is easier *)

val negate : expr -> expr
(** [negate expr] applies the unary operator Neg on [expr] or removes if it is
    already there (double negation)*)

val remove_implies : expr -> expr
(** [remove_implies expr] eliminates all the -> from expr using the equivalency
    A -> B and -A || B, used to get clausal form *)

val distribute_neg : expr -> expr
(** [distribute_neg expr] distributes the unary negation operater across all
    binary operators using DeMorgan's Law. Used to get clausal form. Requires
    expr does not contain implications. *)

val standardize_vars : expr -> expr
(** [standardize_vars expr] renames all the free variables [x] in [exprs] to
    [x1, x2, ...] so no two expressions quanitify over the same free variable,
    as they refer to seperate entities. Requires: [expr] is well-formed. *)

val quantifiers_out : expr -> expr
(** [quantifiers_out expr] moves all of the quanitifers to be the outermost
    level of the expression while preserving equivalence. Requires: all
    quanitifers have different free variable names and negations are moved
    inwards. *)

val skolemize : expr -> expr
(** [skolemize expr] converts [expr] to Skolem Normal Form. Eliminates all
    quantifications from [expr] through 1) substitutes new entities for lone
    existential quantifiers, 2) substitutes new predicates for universal
    quantifiers of existential quantifiers, and 3) elimates universal
    quantifiers. Used to get clausal form. Requires: variables are standardized
    and quantifiers are on outside *)

val drop_universal : expr -> expr
(** [drop_universal expr] removes universal quantifiers from [expr] and makes
    them implicit. Requires: [expr] is in Skolem Normal Form *)

val cnf_form : expr -> expr
(* [cnf_form expr] converts [expr] into Conjunctive Normal Form by distributing
   the ors inwards and removing ands so they become implicit. Requires: [expr]
   does not contain implications or quantifiers and all negations have been
   distributed *)

val separate_ands : expr -> expr list
(* [separate_ands expr] removes ands from [expr] by splitting it into a list of
   expressions so the conjunctions are implicit. Requires: [expr] is in
   Conjunctive Normal Form *)

val separate_ors : expr -> expr list
(* [separate_ors expr] removes ors from [expr] by splitting it into a list of
   expressions so the disjunctions are implicit. Requires: [expr] is in
   Conjunctive Normal Form with implicit ands *)
