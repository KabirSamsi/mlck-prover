open Ast
open Stringop

(*Exception thrown when two incompatible expressions are evaluated*)
exception Incompatible of string

(** len ls computes the length of a list of 'a elemens REQUIRES: ls is a list **)
let rec len ls =
  match ls with
  | [] -> 0
  | h :: t -> 1 + len t

(** rep ls start b1 b2 represents a list-like structure using b1 and b2 as
    sequence boundaries REQUIRES:

    - ls is a list
    - stringify is a function mapping some 'a to a string **)

let rec rep ls start stringify b1 b2 =
  (if start = true then b1 ^ " " else " " (*Opening charasequence cter*))
  ^
  match ls with
  | [] -> b2 (*Base case – closing sequence character*)
  (*Convert the element to a string and concat with the remainder*)
  | h :: t -> stringify h ^ rep t false stringify b1 b2

(** bop_lists f l1 l2 maps a binary operator f2 onto two lists l1, l2 REQUIRES:

    - l1 and l2 are of equal lengths
    - f takes in two parameters and returns an expression **)
let rec bop_lists f l1 l2 =
  if len l1 <> len l2 then [] (*If lists are not of same length*)
  else
    match (l1, l2) with
    (*Map together to form composite bop-based list*)
    | h1 :: t1, h2 :: t2 -> f h1 h2 :: bop_lists f t1 t2
    | _, _ -> [] (*If lists are not of same length*)

(** strset s converts strlist to a String set REQUIRES: strlist is a list of
    strings **)
let rec strset (strlist : string list) =
  match strlist with
  | [] -> []
  | h :: t -> Str h :: strset t

(** add e1 e2 'adds' two expressions together based on their datatypes. Returns
    their sum and raises Incompatible if the two expressions are not of
    compatible summable types (e.g.) 2 + true REQUIRES:

    - e1 and e2 are AST-based expressions **)
let rec add (e1 : expr) (e2 : expr) =
  match e1 with
  | None -> add e2 e1
  | Integer i1 -> (
      (*Case integer*)
      match e2 with
      | Integer i2 -> Integer (i1 + i2)
      | Float i2 -> Float (float_of_int i1 +. i2)
      | None -> e1
      | _ -> raise (Incompatible "Incompatible types"))
  | Float f1 -> (
      (*Case float*)
      match e2 with
      | Float f2 -> Float (f1 +. f2)
      | Integer i2 -> add e2 e1
      (*Operation exists in reverse*)
      | None -> e1
      | _ -> raise (Incompatible "Incompatible types"))
  | Str s1 -> (
      (*Case str*)
      match e2 with
      | Str s2 -> Str (s1 ^ s2)
      | Character c2 -> Str (s1 ^ String.make 1 c2)
      | None -> e1
      | _ -> add e2 e1)
  | Character c1 -> (
      (*Case str*)
      match e2 with
      | Str s2 -> Str (String.make 1 c1 ^ s2)
      | Character c2 -> Str (String.make 1 c1 ^ String.make 1 c2)
      | None -> e1
      | _ -> raise (Incompatible "Incompatible types"))
  | Vector v1 -> (
      (*Case vector – process the two lists using bop_lists and return the
        resultant vector*)
      match e2 with
      | Vector v2 -> Vector (bop_lists add v1 v2)
      | None -> e1
      | _ -> raise (Incompatible "Incompatible types"))
  | _ -> raise (Incompatible "Incompatible types")

(** mult e1 e2 'multiplies' two expressions together based on their datatypes.
    Returns their product and raises Incompatible if the two expressions are not
    of compatible summable types (e.g.) 2 * true REQUIRES:

    - e1 and e2 are AST-based expressions **)
and mult (e1 : expr) (e2 : expr) =
  match e1 with
  | Integer i1 -> (
      (*Case integer*)
      match e2 with
      | Integer i2 -> Integer (i1 * i2)
      | Float i2 -> Float (float_of_int i1 *. i2)
      | Vector _ -> mult e2 e1
      (*Operation exists in reverse*)
      | Str s1 -> Str (mult_string s1 i1)
      | Character c1 -> Str (mult_string (String.make 1 c1) i1)
      | _ -> raise (Incompatible "Incompatible types"))
  | Float f1 -> (
      (*Case float*)
      match e2 with
      | Float f2 -> Float (f1 *. f2)
      | Integer i2 -> mult e2 e1
      (*Operation exists in reverse*)
      | Vector _ -> mult e2 e1
      (*Operation exists in reverse*)
      | _ -> raise (Incompatible "Incompatible types"))
  | Str _ (*Case str*) | Character _ -> (
      (*Case character*)
      match e2 with
      | Integer i2 -> mult e2 e1
      (*Operation exists in reverse*)
      | Vector _ -> mult e2 e1
      (*Operation exists in reverse*)
      | _ -> raise (Incompatible "Incompatible types"))
  | Vector v -> (
      (*Case vector*)
      match e2 with
      (*Scalar multiplication*)
      | Integer _ -> Vector (List.map (fun exp -> mult e2 exp) v)
      | Float _ -> Vector (List.map (fun exp -> mult e2 exp) v)
      | Str _ -> Vector (List.map (fun exp -> mult e2 exp) v)
      (*Dot product of two vectors – map the multiply operator over each term
        and them sum the vector components*)
      | Vector v2 when len v = len v2 ->
          List.fold_left (fun acc x -> add acc x) None (bop_lists mult v v2)
      | _ -> raise (Incompatible "Incompatible types"))
  | _ -> raise (Incompatible "Incompatible types")

(** mult e1 e2 'subtracts' e2 from e1 together based on their datatypes. Returns
    their difference and raises Incompatible if the two expressions are not of
    compatible summable types (e.g.) 2 - true REQUIRES:

    - e1 and e2 are AST-based expressions **)
and sub (e1 : expr) (e2 : expr) =
  match e1 with
  | Integer i1 -> (
      (*Case integer*)
      match e2 with
      | Integer i2 -> Integer (i1 - i2)
      | Float i2 -> Float (float_of_int i1 -. i2)
      | _ -> raise (Incompatible "Incompatible types"))
  | Float f1 -> (
      (*Case float*)
      match e2 with
      | Float f2 -> Float (f1 -. f2)
      | Integer i2 -> Float (f1 -. float_of_int i2)
      | _ -> raise (Incompatible "Incompatible types"))
  | Str s1 -> (
      (*Case str*)
      match e2 with
      (*Remove character from string*)
      | Character c2 ->
          (*Filter out only characters not equal to c2*)
          Str (merge_string (List.filter (fun k -> k <> c2) (to_char_list s1)))
      (*Remove n characters from end of string*)
      | Integer i2 -> Str (String.sub s1 0 (String.length s1 - i2))
      | _ -> raise (Incompatible "Incompatible types"))
  | Vector v -> (
      (*Case vector*)
      match e2 with
      | Vector v2 -> Vector (bop_lists sub v v2)
      | _ -> raise (Incompatible "Incompatible types"))
  | _ -> raise (Incompatible "Incompatible types")

(** div e1 e2 'divides' e2/e1 based on their datatypes. Returns their quotient
    and raises Incompatible if the two expressions are not of compatible
    summable types (e.g.) 2 / true REQUIRES:

    - e1 and e2 are AST-based expressions **)
and div (e1 : expr) (e2 : expr) =
  match e1 with
  | Integer i1 -> (
      (*Case integer*)
      match e2 with
      | Integer i2 -> Integer (i1 / i2)
      | Float i2 -> Float (float_of_int i1 /. i2)
      | _ -> raise (Incompatible "Incompatible types"))
  | Float f1 -> (
      (*Case float*)
      match e2 with
      | Float f2 -> Float (f1 /. f2)
      | Integer i2 -> Float (f1 /. float_of_int i2)
      | _ -> raise (Incompatible "Incompatible types"))
  | Str s1 -> (
      (*Case str*)
      match e2 with
      (*Split on character*)
      | Character c2 -> Set (strset (String.split_on_char c2 s1))
      | _ -> raise (Incompatible "Incompatible types"))
  | Vector v -> (
      (*Case vector*)
      match e2 with
      (*Scalar division*)
      | Integer _ | Float _ | Character _ ->
          Vector (List.map (fun exp -> div exp e2) v)
      | _ -> raise (Incompatible "Incompatible types"))
  | _ -> raise (Incompatible "Incompatible types")

(*at ls search finds the element of a list at the specified index, searching
  smaller portions until the search index reaches 0

  REQUIRES: search is an integer, ls is a list of expressions *)
and at (ls : expr list) (search : int) =
  if search < len ls then
    match ls with
    | [] -> None
    (*Never reached*)
    | h :: t when search = 0 -> h
    (*If n elements have been iterated*)
    | h :: t -> at t (search - 1) (*Recurse over smaller portion*)
  else None

(*div_float e1 e2 divides two integer/float based expressions, or converts ints
  to floats and divides

  REQUIRES: e1 and e2 are both expressions *)
and div_float e1 e2 =
  match (eval e1, eval e2) with
  | Float i1, Float i2 -> Float (i1 /. i2)
  | Integer i1, Float i2 -> Float (float_of_int i1 /. i2)
  | Integer i1, Integer i2 -> Float (float_of_int i1 /. float_of_int i2)
  | Float i1, Integer i2 -> Float (i1 /. float_of_int i2)
  | _, _ -> raise (Incompatible "Incompatible types")

(*Helper function for equivalent_vectors. equivalent_vectors_helper v1 v2 factor
  takes in two vectors and the factor of divison between their first components
  and checks that the remaining components are divisible by the same factor

  REQUIRES: v1 and v2 are both lists of AST-defined expressions *)
and equivalent_vectors_helper (v1 : expr list) (v2 : expr list) (factor : expr)
    =
  match (v1, v2) with
  | [], [] -> Boolean true
  (*If we have traversed the entire vector*)
  (*If current components match the divison factor, keep traversing*)
  | h1 :: t1, h2 :: t2 when eq_eval (div_float h2 h1) factor = Boolean true ->
      equivalent_vectors_helper t1 t2 factor
  | [], _ :: _ | _ :: _, [] | _ :: _, _ :: _ -> Boolean false

(* equivalent_vectors v1 v2 takes in two vectors and checks that they have the
   same direction (constant ratio between magnitudes) *)
and equivalent_vectors (v1 : expr list) (v2 : expr list) =
  match (v1, v2) with
  | [], [] -> Boolean true
  | [], _ :: _ | _ :: _, [] -> Boolean false
  | h1 :: t1, h2 :: t2 -> equivalent_vectors_helper t1 t2 (div h2 h1)

(*cross e1 e2 is vector e1 crossed with e2, after checking that they match the
  type requirements and length requirements*)
and cross (e1 : expr) (e2 : expr) =
  match (e1, e2) with
  (*Check that the vectors are both of length 3*)
  | Vector v1, Vector v2 when len v1 = len v2 && len v1 = 3 ->
      Vector
        [
          (*Build vector representation of the cross product*)
          (*First element*)
          sub (mult (at v1 1) (at v2 2)) (mult (at v1 2) (at v2 1));
          eval
            ((*Second element*)
               UnExpr
               (Inv, sub (mult (at v1 0) (at v2 2)) (mult (at v1 2) (at v2 0))));
          (*Third element*)
          sub (mult (at v1 0) (at v2 1)) (mult (at v1 1) (at v2 0));
        ]
  | Vector v1, Vector v2 ->
      raise (Incompatible "Vectors must both be of length 3")
  | _, _ -> raise (Incompatible "Incompatible types")

(** or_eval e1 e2 takes two evaluated boolean expressions and runs inclusive AND
    on them. Raises Incompatible if types are not compatible (booleans).

    REQUIRES: e1 and e2 are both boolean expressions *)
and and_eval (e1 : expr) (e2 : expr) =
  match e1 with
  | Boolean b1 -> (
      match e2 with
      | Boolean b2 -> Boolean (b1 && b2)
      | _ -> raise (Incompatible "Incompatible types"))
  | _ -> raise (Incompatible "Incompatible types")

(** or_eval e1 e2 takes two evaluated boolean expressions and runs inclusive OR
    on them. Raises Incompatible if types are not compatible (booleans).

    REQUIRES: e1 and e2 are both boolean expressions *)
and or_eval (e1 : expr) (e2 : expr) =
  match e1 with
  | Boolean b1 -> (
      match e2 with
      | Boolean b2 -> Boolean (b1 || b2)
      | _ -> raise (Incompatible "Incompatible types"))
  | _ -> raise (Incompatible "Incompatible types")

(** xor_eval e1 e2 takes two evaluated boolean expressions and runs inclusive OR
    on them. Raises Incompatible if types are non-boolean and therefore
    incompatible.

    REQUIRES: e1 and e2 are two boolean expressions *)
and xor_eval (e1 : expr) (e2 : expr) =
  match e1 with
  | Boolean b1 -> (
      match e2 with
      | Boolean b2 -> Boolean ((b1 && not b2) || (b2 && not b1))
      | _ -> raise (Incompatible "Incompatible types"))
  | _ -> raise (Incompatible "Incompatible types")

(** gt_eval e1 e2 takes two evaluated expressions and compares which one is
    greater based on datatypes. Raises Incompatible if types are not compatible

    REQUIRES: e1 and e2 are two AST-defined expressions *)
and gt_eval (e1 : expr) (e2 : expr) =
  match e1 with
  | Integer i1 -> (
      match e2 with
      | Integer i2 -> Boolean (i1 > i2)
      | Float i2 -> Boolean (float_of_int i1 > i2)
      | _ -> raise (Incompatible "Incompatible with integer"))
  | Float f1 -> (
      match e2 with
      | Float f2 -> Boolean (f1 > f2)
      | Integer i2 -> gt_eval e2 e1
      | _ -> raise (Incompatible "Incompatible with float"))
  | Boolean b1 -> (
      match e2 with
      | Boolean b2 -> Boolean (b1 > b2)
      | _ -> raise (Incompatible "Incompatible with boolean"))
  | Str s1 -> (
      match e2 with
      | Str s2 -> Boolean (s1 > s2)
      | Character s2 -> Boolean (String.make 1 s2 > s1)
      | _ -> raise (Incompatible "Incompatible with string"))
  | Character c1 -> (
      match e2 with
      | Character c2 -> Boolean (c1 > c2)
      | Str c2 -> Boolean (String.make 1 c1 > c2)
      | _ -> raise (Incompatible "Incompatible with char"))
  | _ -> raise (Incompatible "Incompatible types")

(** eq_eval e1 e2 takes two evaluated expressions and compares if they are
    'equal' given compatible datatypes. Raises Incompatible if types are not
    compatible.

    REQUIRES: e1 and e2 are two AST-defined expressions **)
and eq_eval (e1 : expr) (e2 : expr) =
  match eval e1 with
  | Integer i1 -> (
      (*Case integer - compatible with int/float*)
      match eval e2 with
      | Integer i2 -> Boolean (i1 = i2)
      | Float i2 -> Boolean (float_of_int i1 = i2)
      | _ -> raise (Incompatible "Incompatible with integer"))
  | Float f1 -> (
      (*Case float – compatible with int/float*)
      match eval e2 with
      | Float f2 -> Boolean (f1 = f2)
      | Integer f2 -> eq_eval e2 e1 (*Reverse operation already exists*)
      | _ -> raise (Incompatible "Incompatible with float"))
  | Boolean b1 -> (
      (*Case boolean – compatible with boolean*)
      match eval e2 with
      | Boolean b2 -> Boolean (b1 = b2)
      | _ -> raise (Incompatible "Incompatible with boolean"))
  | Str s1 -> (
      (*Case string - compatible with string/char*)
      match eval e2 with
      | Str s2 -> Boolean (s1 = s2)
      | Character s2 -> Boolean (String.make 1 s2 = s1)
      | _ -> raise (Incompatible "Incompatible with string"))
  | Character c1 -> (
      (*Case character – compatible with string*)
      match eval e2 with
      | Character c2 -> Boolean (c1 = c2)
      | Str c2 -> eq_eval e2 e1
      | _ -> raise (Incompatible "Incompatible with char"))
  | Vector v1 -> (
      match eval e2 with
      | Vector v2 -> equivalent_vectors v1 v2
      | _ -> raise (Incompatible "Incompatible with vector"))
  | Set s1 -> (
      match eval e2 with
      | Set s2 -> Boolean (Sets.equals e1 e2)
      | _ -> raise (Incompatible "Incompatible with set"))
  (*Simplify expressions, case never reached*)
  | _ -> (
      match e2 with
      | None -> Boolean true
      | _ -> Boolean false)

(*simplify_list ls evaluates and simplifies a list of expressions REQUIRES: ls
  is a valid list containing AST-defined expressions*)
and simplify_list (ls : expr list) =
  match ls with
  | [] -> []
  (*Evaluates the current expression, simplifies remainder*)
  | h :: t -> eval h :: simplify_list t

(** eval e evaluates or simplifies an expression until it can step no further,
    reaching the most baseline form

    REQUIRES: e is a valid expression defined in the AStT *)
and eval (e : expr) =
  match e with
  | BopExpr (bop, e1, e2) -> (
      (*Evaluate all bop expressions*)
      match bop with
      | Add -> add (eval e1) (eval e2)
      | Sub -> sub (eval e1) (eval e2)
      | Mult -> mult (eval e1) (eval e2)
      | Cross -> cross (eval e1) (eval e2)
      | Div -> div (eval e1) (eval e2)
      | And -> and_eval (eval e1) (eval e2)
      | Or -> or_eval (eval e1) (eval e2)
      | Xor -> xor_eval (eval e1) (eval e2)
      | Eq -> eq_eval (eval e1) (eval e2)
      | Gt -> gt_eval (eval e1) (eval e2)
      (*Utilize earlier functions to optimize further comparisons*)
      (*Lt: Evaluate that e1 is Not greater than and not equal to e2*)
      | Lt ->
          eval
            (BopExpr
               ( And,
                 eval (UnExpr (Not, eval (BopExpr (Gt, e1, e2)))),
                 eval (UnExpr (Not, eval (BopExpr (Eq, e1, e2)))) ))
      (*Neq: Evaluate that e1 is Not equal to e2*)
      | Neq -> eval (UnExpr (Not, eq_eval (eval e1) (eval e2)))
      (*Geq: Evaluate that e1 is Not less than e2*)
      | Geq -> eval (UnExpr (Not, eval (BopExpr (Lt, eval e1, eval e2))))
      (*Leq: Evaluate that e1 is Not greater than e2*)
      | Leq -> eval (UnExpr (Not, eval (BopExpr (Gt, eval e1, eval e2))))
      | Setadd -> Sets.add (eval e1) (eval e2)
      | Setremove -> Sets.remove (eval e1) (eval e2)
      | Union -> Sets.union (eval e1) (eval e2)
      | Intersection -> Sets.intersect (eval e1) (eval e2)
      | Subset -> Boolean (Sets.is_subset (eval e1) (eval e2))
      | Setequals -> Boolean (Sets.equals (eval e1) (eval e2)))
  | UnExpr (un, e) -> (
      (*Evaluate all unary expressions*)
      match un with
      (*Case Not - negate the boolean expression*)
      | Not -> (
          match eval e with
          | Boolean b -> Boolean (not b)
          | _ -> raise (Incompatible "Cannot evaluate non-bool expression"))
      | Inv -> (
          (*Unary inverse operation*)
          match eval e with
          | Boolean b -> Boolean (not b)
          | Integer i -> Integer (~-1 * i)
          | Float f -> Float (-1. *. f)
          | Str s -> Str (rev_string s)
          | _ -> raise (Incompatible "No inverse exists")))
  (*List-based structures – evaluate and simplify lists*)
  | Vector v -> Vector (simplify_list v)
  | Set s ->
      let set_add xs x = if List.mem x xs then xs else x :: xs in
      Set (List.rev (List.fold_left set_add [] (simplify_list s)))
  | _ -> e

(** parse str parses a given string to an AST-based expression REQUIRES: str is
    a lexable string *)
let parse str =
  let lexbuf = Lexing.from_string str in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** string_of_unop unop computes a string representation of an unary operator
    REQUIRES: unop is a valid unary operator defined in the AST *)
let string_of_unop unop =
  match unop with
  | Not -> "!"
  | Inv -> "Inverse of "

(*string_of_binop binop computes a string representation of a binary operator
  REQUIRES: binop is a valid binary operator defined in the AST *)
let string_of_binop binop =
  match binop with
  | Add -> " + "
  | Sub -> " - "
  | Mult -> " * "
  | Div -> " / "
  | And -> " ʌ "
  | Or -> " v "
  | Xor -> " ⊕ "
  | Eq -> " = "
  | Neq -> " ≠ "
  | Gt -> " > "
  | Lt -> " < "
  | Geq -> " ≥ "
  | Leq -> " ≤ "
  | Cross -> " x "
  | Setadd -> " {ADD}"
  | Setremove -> " {REMOVE} "
  | Union -> " UNION "
  | Intersection -> " INTERSECTION "
  | Subset -> " {SUBSET}"
  | Setequals -> " {=}"

(*to_string expr computes a string representation of an expression REQUIRES:
  expr is a valid expression defined in the AST *)
let rec to_string expr =
  match expr with
  | None -> "<none>"
  | Integer i -> string_of_int i
  | Str s -> "\"" ^ s ^ "\""
  | Character c -> "'" ^ String.make 1 c ^ "'"
  | Boolean b -> string_of_bool b
  | Float f -> string_of_float f
  | Set s -> rep s true to_string "{" "}"
  | Vector v -> rep v true to_string "<" ">"
  | UnExpr (op, e) -> string_of_unop op ^ to_string e
  | BopExpr (op, e1, e2) -> to_string e1 ^ string_of_binop op ^ to_string e2

(*interp str parses, evaluates and then outputs the string-formatted expression
  that the user enters.*)
let interp str = str |> parse |> eval |> to_string
