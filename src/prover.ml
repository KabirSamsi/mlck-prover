(* existential and universal quantifiers *)
type quantifier =
  | All
  | Exists

type binop =
  | Implies (* -> *)
  | And
  | Or

type unop = Not

type var =
  | Free of string
  | Entity of string
  | Function of string * var list

(* predicates can be defined as a function of a list of entities or free
   variables, which be quanitifed by a quantifier statement on those free
   variables.

   ex. The predicate phrase [exists x. (Dog(x) & Pet(x, John))] should have the
   form [QuantExpr (Exists, Free "x", BinExpr (And, PredExpr ("Dog", [Free
   "x"]), PredExpr ("Pet", [Free "x"; Entity "John"])))] *)
type expr =
  | UnExpr of unop * expr
  | BinExpr of binop * expr * expr
  | PredExpr of string * var list
  | QuantExpr of quantifier * var * expr

type constraints = (var * var) list

let p1 =
  QuantExpr
    ( Exists,
      Free "x",
      BinExpr
        ( And,
          PredExpr ("Dog", [ Free "x" ]),
          PredExpr ("Pet", [ Free "x"; Entity "John" ]) ) )

let p2 =
  QuantExpr
    ( All,
      Free "x",
      BinExpr
        ( Implies,
          PredExpr ("P", [ Free "x" ]),
          BinExpr
            ( And,
              QuantExpr
                ( Exists,
                  Free "y",
                  QuantExpr
                    (All, Free "x", PredExpr ("Q", [ Free "x"; Free "y" ])) ),
              PredExpr ("F", [ Function ("R", [ Free "x" ]) ]) ) ) )

let p3 =
  QuantExpr
    ( All,
      Free "x",
      BinExpr
        ( Or,
          QuantExpr
            ( Exists,
              Free "y",
              BinExpr
                ( And,
                  PredExpr ("Animal", [ Free "y" ]),
                  UnExpr (Not, PredExpr ("Loves", [ Free "x"; Free "y" ])) ) ),
          QuantExpr
            (Exists, Free "y", PredExpr ("Loves", [ Free "y"; Free "x" ])) ) )

let p4 =
  QuantExpr
    ( All,
      Free "a",
      QuantExpr
        ( Exists,
          Free "b",
          QuantExpr
            ( All,
              Free "c",
              QuantExpr
                ( Exists,
                  Free "d",
                  PredExpr ("P", [ Free "a"; Free "b"; Free "c"; Free "d" ]) )
            ) ) )

(* A || (B & C) = (A || B) & (A || C) *)
(* ((B & C) || (D & E)) = ((B & C) || D) & ((B & C) || E)) = (B || D & C || D &
   B || E & C || E) *)
let p5 =
  BinExpr
    ( Or,
      BinExpr (And, PredExpr ("B", []), PredExpr ("C", [])),
      BinExpr (And, PredExpr ("D", []), PredExpr ("E", [])) )

(* p2 |> to_string

   List.hd (standardize_vars [p2]) |> to_string

   p2 |> negate |> remove_implies |> distribute_neg |> to_string

   List.hd (standardize_vars [p2 |> negate |> remove_implies |> distribute_neg])
   |> quantifiers_out |> to_string

   p3 |> to_string;;

   List.hd (standardize_vars [p3]) |> to_string ;;

   List.hd (standardize_vars [p3]) |> quantifiers_out |> skolemize |> to_string

   List.hd (standardize_vars [p4]) |> quantifiers_out |> skolemize |> to_string
   ;; *)

let verbose = ref true
let print_verbose s = if !verbose then print_endline s else ()

let toggle_verbosity () =
  verbose := not !verbose;
  print_endline ("Set verbosity to " ^ string_of_bool !verbose)

let rec var_to_string v =
  match v with
  | Free x -> x
  | Entity x -> x
  | Function (f, vs) ->
      f ^ "(" ^ String.concat ", " (List.map var_to_string vs) ^ ")"

let rec to_string_helper expr tbs =
  let new_expr =
    match expr with
    | UnExpr (Not, e) -> "¬" ^ to_string_helper e (tbs + 1)
    | BinExpr (Implies, e1, e2) ->
        to_string_helper e1 (tbs + 1) ^ " -> " ^ to_string_helper e2 (tbs + 1)
    | BinExpr (And, e1, e2) ->
        to_string_helper e1 (tbs + 1) ^ " ∧ " ^ to_string_helper e2 (tbs + 1)
    | BinExpr (Or, e1, e2) ->
        to_string_helper e1 (tbs + 1) ^ " ∨ " ^ to_string_helper e2 (tbs + 1)
    | QuantExpr (All, v, e) ->
        "∀" ^ var_to_string v ^ ". " ^ to_string_helper e (tbs + 1)
    | QuantExpr (Exists, v, e) ->
        "∃" ^ var_to_string v ^ ". " ^ to_string_helper e (tbs + 1)
    | PredExpr (f, vs) ->
        if vs = [] then f
        else f ^ "(" ^ String.concat ", " (List.map var_to_string vs) ^ ")"
  in
  match (expr, tbs) with
  | _, 0 -> new_expr
  | BinExpr _, _ -> "(" ^ new_expr ^ ")"
  | _, _ -> new_expr

(* p1 |> to_string |> print_endline *)

(* all x. (P(x) -> (exists y. all x. Q(x, y) & Q(x))) *)
let to_string s = to_string_helper s 0

let remove_duplicates ls =
  let cons_uniq xs x = if List.mem x xs then xs else x :: xs in
  List.rev (List.fold_left cons_uniq [] ls)

let rec variables_helper expr =
  let rec get_inner v =
    match v with
    | Function (_, vs) -> v :: List.flatten (List.map get_inner vs)
    | _ -> [ v ]
  in
  match expr with
  | UnExpr (_, e) -> variables_helper e
  | BinExpr (_, e1, e2) -> variables_helper e1 @ variables_helper e2
  | PredExpr (_, vs) -> List.flatten (List.map get_inner vs)
  | QuantExpr (_, v, e) -> v :: variables_helper e

let variables expr = expr |> variables_helper |> remove_duplicates

(* bindings are free variables that are quantified (and thus bound) in this
   scope*)
let rec get_unbound vs bindings =
  match vs with
  | [] -> []
  | Free x :: t ->
      if List.mem (Free x) bindings then get_unbound t bindings
      else Free x :: get_unbound t bindings
  | Entity _ :: t -> get_unbound t bindings
  | Function (_, rec_vs) :: t ->
      get_unbound rec_vs bindings @ get_unbound t bindings

let rec unbound_helper expr bindings =
  match expr with
  | UnExpr (_, e) -> unbound_helper e bindings
  | BinExpr (_, e1, e2) ->
      unbound_helper e1 bindings @ unbound_helper e2 bindings
  | PredExpr (_, vs) -> get_unbound vs bindings
  | QuantExpr (_, v, e) -> (
      match v with
      | Free x -> unbound_helper e (Free x :: bindings)
      | _ -> failwith "quantification should be over one free variable")

let unbound_variables expr = remove_duplicates (unbound_helper expr [])

let rec check_quantifiers expr =
  match expr with
  | UnExpr (_, e) -> check_quantifiers e
  | BinExpr (_, e1, e2) -> check_quantifiers e1 && check_quantifiers e2
  | PredExpr _ -> true (* predicates cannot have inner quanitifiers *)
  | QuantExpr (_, Free v, e) -> check_quantifiers e
  | QuantExpr (_, _, e) -> false (* quantifies over an entity *)

let is_well_formed expr = check_quantifiers expr && unbound_variables expr = []

(* helper function that returns variable v with all instances of y substituted
   with x, {x/y} *)
let rec substitute_var v x y =
  match v with
  | v when v = y -> x
  | Function (f, vs) -> Function (f, List.map (fun v -> substitute_var v x y) vs)
  | _ -> v

let rec substitute_expr expr x y =
  match expr with
  | UnExpr (op, e) -> UnExpr (op, substitute_expr e x y)
  | BinExpr (op, e1, e2) ->
      BinExpr (op, substitute_expr e1 x y, substitute_expr e2 x y)
  | PredExpr (p, vs) -> PredExpr (p, List.map (fun v -> substitute_var v x y) vs)
  | QuantExpr (op, v, e) ->
      (* potential case of trying to substitute a variable that is already being
         bounded in scope? *)
      if v = x then expr
      else QuantExpr (op, substitute_var v x y, substitute_expr e x y)

let negate expr =
  match expr with
  | UnExpr (Not, e) -> e
  | _ -> UnExpr (Not, expr)

let rec remove_implies expr =
  match expr with
  (* A -> B = -A || B *)
  | BinExpr (Implies, e1, e2) ->
      BinExpr (Or, e1 |> negate |> remove_implies, e2 |> remove_implies)
  (* recursively check within operator expressions *)
  | UnExpr (unop, e) -> UnExpr (unop, remove_implies e)
  | QuantExpr (quantifier, v, e) -> QuantExpr (quantifier, v, remove_implies e)
  | BinExpr (binop, e1, e2) ->
      BinExpr (binop, remove_implies e1, remove_implies e2)
  (* Predicates have no inner expressions; no need for checking *)
  | PredExpr _ -> expr

let rec distribute_neg expr =
  match expr with
  | UnExpr (Not, BinExpr (Implies, _, _)) ->
      failwith "implications should be elimated before distributing negations"
  (* -(-A) = A *)
  | UnExpr (Not, UnExpr (Not, e)) -> distribute_neg e
  (* -(A & B) = -A || -B*)
  | UnExpr (Not, BinExpr (And, e1, e2)) ->
      BinExpr
        (Or, e1 |> negate |> distribute_neg, e2 |> negate |> distribute_neg)
  (* -(A & B) = -A || -B*)
  | UnExpr (Not, BinExpr (Or, e1, e2)) ->
      BinExpr
        (And, e1 |> negate |> distribute_neg, e2 |> negate |> distribute_neg)
  (* -exists x. P(x) = all x. -P(x)*)
  | UnExpr (Not, QuantExpr (Exists, v, e)) ->
      QuantExpr (All, v, e |> negate |> distribute_neg)
  (* -all x. P(x) = exists x. -P(x)*)
  | UnExpr (Not, QuantExpr (All, v, e)) ->
      QuantExpr (Exists, v, e |> negate |> distribute_neg)
  (* recursively check within operator expressions *)
  | QuantExpr (quantifier, v, e) -> QuantExpr (quantifier, v, distribute_neg e)
  | BinExpr (binop, e1, e2) ->
      BinExpr (binop, distribute_neg e1, distribute_neg e2)
  (* neg already push most inwards to predicate *)
  | UnExpr (Not, PredExpr _) | PredExpr _ -> expr

let rec rename_helper count_ref expr =
  match expr with
  | UnExpr (op, e) -> UnExpr (op, rename_helper count_ref e)
  | BinExpr (op, e1, e2) ->
      BinExpr (op, rename_helper count_ref e1, rename_helper count_ref e2)
  | PredExpr (p, vs) -> expr (* predicates cannot have inner quanitifiers *)
  | QuantExpr (op, Free v, e) ->
      count_ref := !count_ref + 1;
      let new_v = v ^ string_of_int !count_ref in
      let new_e = substitute_expr e (Free new_v) (Free v) in
      QuantExpr (op, Free new_v, rename_helper count_ref new_e)
  | QuantExpr _ -> failwith "quantification should be over one free variable"

let standardize_vars expr =
  let counter = ref 0 in
  rename_helper counter expr

let rec quantifiers_out expr =
  match expr with
  | BinExpr (op, p, QuantExpr (quant, var, q_var)) -> begin
      let new_p = quantifiers_out p in
      let new_q = quantifiers_out q_var in
      let new_expr = BinExpr (op, new_p, new_q) in
      (* walk back up again if changes were made *)
      match (new_p, new_q) with
      | QuantExpr _, _ | _, QuantExpr _ ->
          QuantExpr (quant, var, quantifiers_out new_expr)
      | _ -> QuantExpr (quant, var, new_expr)
    end
  | BinExpr (op, QuantExpr (quant, var, q_var), p) -> begin
      let new_p = quantifiers_out p in
      let new_q = quantifiers_out q_var in
      let new_expr = BinExpr (op, new_q, new_p) in
      match (new_p, new_q) with
      | QuantExpr _, _ | _, QuantExpr _ ->
          QuantExpr (quant, var, quantifiers_out new_expr)
      | _ -> QuantExpr (quant, var, new_expr)
    end
  | BinExpr (op, p, q) -> begin
      let new_p = quantifiers_out p in
      let new_q = quantifiers_out q in
      let new_expr = BinExpr (op, new_p, new_q) in
      match (new_p, new_q) with
      | QuantExpr _, _ | _, QuantExpr _ -> quantifiers_out new_expr
      | _ -> new_expr
    end
  | QuantExpr (quant, var, e) -> QuantExpr (quant, var, quantifiers_out e)
  (* unary and predicate dont have nested quanitifers *)
  | e -> e

let rec skolemize_helper expr all_bindings =
  match expr with
  | QuantExpr (Exists, Free v, e) ->
      let new_e =
        if all_bindings = [] then substitute_expr e (Entity v) (Free v)
        else
          let skolem_function = Function ("S_" ^ v, all_bindings) in
          substitute_expr e skolem_function (Free v)
      in
      skolemize_helper new_e all_bindings
  | QuantExpr (All, Free v, e) ->
      QuantExpr (All, Free v, skolemize_helper e (Free v :: all_bindings))
  | QuantExpr _ ->
      failwith
        "only free variables should be quantified; expression is not well \
         formed"
  | e -> e
(* quanitifers are all on outside, so don't need to check others *)

let skolemize expr = skolemize_helper expr []

let rec drop_universal expr =
  match expr with
  | QuantExpr (All, var, e) -> drop_universal e
  | QuantExpr (Exists, _, _) ->
      failwith
        "Existential quantifier in expr; it should be in Skolem Normal Form"
  | _ -> expr (* quantifiers are all on outside *)

let rec cnf_form expr =
  match expr with
  | BinExpr (Or, e1, e2) -> begin
      match (e1, e2) with
      | p, BinExpr (And, q, r) ->
          BinExpr
            (And, cnf_form (BinExpr (Or, p, q)), cnf_form (BinExpr (Or, p, r)))
      | BinExpr (And, q, r), p ->
          BinExpr
            (And, cnf_form (BinExpr (Or, q, p)), cnf_form (BinExpr (Or, r, p)))
      | _ -> BinExpr (Or, cnf_form e1, cnf_form e2)
    end
  | BinExpr (And, e1, e2) -> BinExpr (And, cnf_form e1, cnf_form e2)
  (* predicates and negations have no nested ands/ors *)
  | PredExpr _ | UnExpr _ -> expr
  | QuantExpr _ -> failwith "there should be no quantifiers in the expression"
  | BinExpr (Implies, _, _) ->
      failwith "there should be no implications in the expression"

let rec separate_binop_helper op expr =
  match expr with
  | BinExpr (o, e1, e2) when o = op ->
      separate_binop_helper op e1 @ separate_binop_helper op e2
  | e -> [ e ]
(* CNF form; all conjunctions are on outside *)

let separate_ands expr = remove_duplicates (separate_binop_helper And expr)
let separate_ors expr = remove_duplicates (separate_binop_helper Or expr)

let print_exprs exprs =
  print_verbose ("  " ^ String.concat "\n  " (List.map to_string exprs))

let print_substitions subs =
  let sub_strings =
    List.map
      (fun (x, y) -> "{" ^ var_to_string x ^ " / " ^ var_to_string y ^ "}")
      subs
  in
  print_verbose ("  [ " ^ String.concat ", " sub_strings ^ " ]")

let clausal_form expr =
  let steps =
    [
      "Eliminate implications";
      "Move negations inwards";
      "Standardize variables";
      "Move quantifiers outwards";
      "Eliminate existential quantifiers";
      "Drop universal quantifiers";
      "Move disjunctions inwards";
    ]
  in
  (* pipe function with printing output as additional side effect *)
  let count = ref 0 in
  let ( >@ ) x f =
    let c = !count in
    count := !count + 1;
    let f_x = x |> f in
    ( f_x |> to_string
    |> ( ^ )
         ("Step " ^ string_of_int !count ^ " - " ^ List.nth steps c ^ ":\n  ")
    |> fun x ->
      print_verbose x;
      print_verbose "" );
    f_x
  in
  print_verbose
    ("\n---\n\nConverting to clausal form:\n  " ^ to_string expr ^ "\n");
  let cnf =
    expr >@ remove_implies >@ distribute_neg >@ standardize_vars
    >@ quantifiers_out >@ skolemize >@ drop_universal >@ cnf_form
  in
  let new_exprs = separate_ands cnf in
  print_verbose "\nDerived expressions:";
  print_exprs new_exprs;
  new_exprs

(* helper function that joins expressions with conjunction to get an equivalent
   singular expression *)
let rec join_expressions op exprs =
  match exprs with
  | [] -> None
  | h :: t -> begin
      match join_expressions op t with
      | Some e -> Some (BinExpr (op, h, e))
      | None -> Some h
    end

(* perform all substitutions {x/y} in constraints in the variable list vs *)
let rec apply_var_constraints vs constraints =
  match constraints with
  | [] -> vs
  | (x, y) :: t -> List.map (fun v -> substitute_var v x y) vs

(* perform all substitutions {x/y} in constraints in the expression list
   exprs *)
let rec apply_exprs_constraints exprs constraints =
  match constraints with
  | [] -> exprs
  | (x, y) :: t -> List.map (fun e -> substitute_expr e x y) exprs

(* unify two variables and return a list of variable pairs [a, b] that result in
   equality when all instances of b are replaced with a *)
let rec unify_vars v1 v2 constraints =
  match (v1, v2) with
  (* if both entities, they must be the same, else fail *)
  | Entity e1, Entity e2 -> if e1 = e2 then Some [] else None
  | Entity e, x | x, Entity e -> Some [ (Entity e, x) ]
  | Free x1, Free x2 -> Some [ (Free x1, Free x2) ]
  | Free x, Function (f, vs) | Function (f, vs), Free x ->
      Some [ (Free x, Function (f, vs)) ]
  | Function (f1, vs1), Function (f2, vs2) ->
      (* cannot unify if they have different predicate symbol *)
      if f1 <> f2 then None else unify_vars_list vs1 vs2 constraints

(* unify a set of equalities of predicate variables and return constraints *)
and unify_vars_list vs1 vs2 constraints =
  if List.length vs1 <> List.length vs2 then None
  else
    match (vs1, vs2) with
    | [], [] ->
        Some [] (* both have no more arguments to unify, so we're done *)
    | h1 :: t1, h2 :: t2 -> begin
        let new_constraints = unify_vars h1 h2 constraints in
        match (new_constraints, constraints) with
        | None, _ | _, None -> None
        | Some cons, Some old -> (
            (* substitute in newly found constraints *)
            let new_t1 = apply_var_constraints t1 cons in
            let new_t2 = apply_var_constraints t2 cons in
            let constraints = cons @ old in

            match unify_vars_list new_t1 new_t2 (Some constraints) with
            | None -> None
            | Some future -> Some (future @ constraints))
      end
    | [], _ | _, [] -> failwith "unreachable"

(* raises exception if cannot continue with this unification *)
exception Inconclusive

(* raises exception if we have reached a contradiction *)
exception Contradiction

let rec unify_expr_pair e1 e2 =
  (* print_endline "\nchecking pair to unify: "; print_exprs [ e1; e2 ]; *)
  (* if identical, no additional substitutions are needed to unify *)
  if e1 = e2 then Some []
  else
    match (e1, e2) with
    | PredExpr (p1, vs1), PredExpr (p2, vs2) ->
        (* if they have different predicate symbols, then inconclusive *)
        if p1 != p2 then raise Inconclusive
        else unify_vars_list vs1 vs2 (Some [])
    | UnExpr (Not, PredExpr (p1, vs1)), UnExpr (Not, PredExpr (p2, vs2)) ->
        (* if they have different predicate symbols, then inconclusive *)
        if p1 != p2 then raise Inconclusive
        else unify_vars_list vs1 vs2 (Some [])
    | PredExpr (p, vs), UnExpr (Not, PredExpr (not_p, not_vs))
    | UnExpr (Not, PredExpr (not_p, not_vs)), PredExpr (p, vs) -> begin
        (* if their negations are unifiable, then this is a contradiction*)
        match unify_vars_list vs not_vs (Some []) with
        | Some _ -> None
        | None | (exception Inconclusive) -> raise Inconclusive
      end
    | _ ->
        failwith
          ("expressions should be in implicit CNF form: " ^ to_string e1
         ^ to_string e2)

let same_elements l1 l2 =
  let contains_all s b = List.fold_left (fun a x -> a && List.mem x b) true s in
  contains_all l1 l2 && contains_all l2 l1

let rec resolvable_pair (l1 : expr list) (l2 : expr list) =
  let unifiable e1 e2 =
    (* print_endline ("\nchecking if pair is unifiable: " ^ to_string e1 ^ " and
       " ^ to_string e2); *)
    match unify_expr_pair e1 (negate e2) with
    | Some _ -> true
    | None | (exception Inconclusive) -> false
  in
  match l1 with
  | [] -> None
  | e1 :: t ->
      let e2s = List.filter (unifiable e1) l2 in
      if e2s = [] then resolvable_pair t l2
      else
        (* print_endline "resolvable pairs found: "; print_exprs [ e1; List.hd
           e2s ]; *)
        Some (e1, List.hd e2s)

(* if you have a formula alpha or phi and another formula not psi or beta, and
   you can unify phi and psi with unifier theta, then you're allowed to conclude
   alpha or beta with the substitution theta applied to it. P || Q and -Q -> P P
   || Q and -Q || R -> P || R

   simplifies the expressions e1 and e2 into new expressions and returns new
   constraints, if possible *)
let resolve_pair (l1 : expr list) (l2 : expr list) =
  let l1 = remove_duplicates l1 in
  let l2 = remove_duplicates l2 in
  (* P & P = P *)
  if same_elements l1 l2 then (l1, [], [])
  else
    match (l1, l2) with
    | [ e1 ], [ e2 ] -> begin
        (* print_endline "unifying singletons"; *)
        match unify_expr_pair e1 e2 with
        | Some c ->
            (* print_endline "unifiable with constraints: "; print_substitions
               c; *)
            (apply_exprs_constraints l1 c, apply_exprs_constraints l2 c, c)
        | None ->
            print_verbose "Arrived at a contradiction.";
            raise Contradiction
      end
    | _ ->
        (* (P || Q) & (-Q || R) = (P || R) *)
        let constraints, e1, e2 =
          match resolvable_pair l1 l2 with
          | None -> raise Inconclusive
          | Some (e1, e2) -> (
              match unify_expr_pair e1 (negate e2) with
              | Some c -> (c, e1, e2)
              | None -> raise Contradiction)
        in
        let rem_l1 =
          apply_exprs_constraints (List.filter (( <> ) e1) l1) constraints
        in
        let rem_l2 =
          apply_exprs_constraints (List.filter (( <> ) e2) l2) constraints
        in
        (rem_l1, rem_l2, constraints)

(* Do one round of going through all the pairs and trying to resolve *)
let resolve_exprs exprs =
  print_verbose "\n---\n\nStarting round of resolution: ";
  let expr_array = Array.of_list exprs in
  let len = Array.length expr_array in
  let constraints = ref [] in
  for i = 0 to len - 1 do
    for j = i + 1 to len - 1 do
      let l1 = expr_array.(i) in
      let l2 = expr_array.(j) in
      let () =
        match (join_expressions Or l1, join_expressions Or l2) with
        | Some j1, Some j2 ->
            print_verbose "\nAttempting to resolve pair: ";
            print_exprs [ j1; j2 ]
        | _ -> ()
      in
      let new_l1, new_l2, new_constraints =
        match resolve_pair l1 l2 with
        | exception Inconclusive ->
            print_verbose "Unable to resolve.";
            (l1, l2, [])
        | new_l1, new_l2, new_constraints ->
            print_verbose "Successfully resolved.\nDerived new constraints: ";
            print_substitions new_constraints;
            let () =
              match
                (join_expressions Or new_l1, join_expressions Or new_l2)
              with
              | Some j1, Some j2 ->
                  print_verbose "Derived new expressions: ";
                  print_exprs [ j1; j2 ]
              | Some j, _ | _, Some j ->
                  print_verbose "Derived new expressions: ";
                  print_exprs [ j ]
              | _ -> ()
            in
            (new_l1, new_l2, new_constraints)
      in
      expr_array.(i) <- new_l1;
      expr_array.(j) <- new_l2;
      constraints := new_constraints @ !constraints
    done
  done;
  let exprs_list = Array.to_list expr_array in
  print_verbose "\n---\n\nCompleted round of resolution: ";
  print_verbose "\nDerived expressions: ";
  print_exprs
    (List.map
       (fun x ->
         match x with
         | Some y -> y
         | None -> failwith "unreachable")
       (List.filter (( <> ) None) (List.map (join_expressions Or) exprs_list)));
  print_verbose "\nDerived substititions: ";
  print_substitions !constraints;
  (exprs_list, !constraints)

let rec unify_exprs_helper exprs constraints =
  if exprs = [] then constraints
    (* no more expressions to unify, so we're done*)
  else
    let new_exprs, new_constraints = resolve_exprs exprs in
    if
      new_exprs = exprs (* nothing has changed; no further proofs can be done *)
    then constraints
    else
      (* continue trying to resolve*)
      let cleaned_exprs = List.filter (( <> ) []) new_exprs in
      (* remove empty *)
      let applied_exprs =
        List.map
          (fun l -> apply_exprs_constraints l new_constraints)
          cleaned_exprs (*apply new constraints*)
      in
      print_verbose "\n Applied new constraints: ";
      print_exprs
        (List.map
           (fun x ->
             match x with
             | Some y -> y
             | None -> failwith "unreachable")
           (List.filter (( <> ) None)
              (List.map (join_expressions Or) applied_exprs)));
      let all_constraints = remove_duplicates (new_constraints @ constraints) in
      unify_exprs_helper applied_exprs all_constraints
(* todo : print this stuff *)

let unify_exprs exprs =
  print_verbose "\n---\n\nExpressions to unify: ";
  print_exprs exprs;
  let clauses = List.map separate_ors exprs in
  match unify_exprs_helper clauses [] with
  | constraints ->
      print_verbose
        "\n\
         ---\n\n\
         Successfully unified the expressions with the following \
         substitutions:  ";
      print_substitions constraints;
      Some constraints
  | exception Contradiction ->
      print_verbose "\n---\n\nCould not unify the expressions. ";
      None

let prove givens expr =
  print_verbose "\nGiven statements: ";
  print_exprs givens;
  print_verbose
    "\n\
     For contradiction, assume the negation of the statement we want to prove. ";
  print_exprs [ negate expr ];
  let exprs =
    (match join_expressions And (negate expr :: givens) with
    | Some g -> g
    | None -> failwith "There are no given statements")
    |> clausal_form
  in
  match unify_exprs exprs with
  | Some _ | (exception Inconclusive) ->
      print_verbose "\nCould not prove the expression.\n";
      false
  | None ->
      print_verbose
        ("\nFrom proof by contradiction, the expression " ^ to_string expr
       ^ " was shown to be true.\n");
      true

let equivalent expr1 expr2 = prove [ expr1 ] expr2 && prove [ expr2 ] expr1
