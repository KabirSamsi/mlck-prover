open Ast

exception InvalidSet of string

let rec is_in_helper (i : expr) (l : expr list) =
  match l with
  | h :: t -> if i = h then true else is_in_helper i t
  | [] -> false

let is_in (i : expr) (s : expr) =
  match s with
  | Set l -> Ast.Boolean (is_in_helper i l)
  | _ -> raise (InvalidSet "Not a set")

let is_in_boolean (i : expr) (s : expr) =
  match is_in i s with
  | Boolean b -> b
  | _ -> failwith "Impossible"

let rec remove_duplicates_helper l =
  match l with
  | h :: t ->
      if is_in_helper h t then remove_duplicates_helper t
      else h :: remove_duplicates_helper t
  | [] -> []

let rec remove_duplicates s =
  match s with
  | Set l -> Ast.Set (remove_duplicates_helper l)
  | _ -> raise (InvalidSet "Not a set")

(* Raised when the set is invalid *)
let empty () = Ast.Set []
let is_empty s = s = Ast.Set []

let add (i : expr) (s : expr) : expr =
  match s with
  | Set l -> if is_in_boolean i s then s else Ast.Set (i :: l)
  | _ -> raise (InvalidSet "Not a set")

let rec remove (i : expr) (s : expr) : expr =
  match s with
  | Set l -> Ast.Set (List.filter (fun x -> x <> i) l)
  | _ -> raise (InvalidSet "Not a set")

let union (s1 : expr) (s2 : expr) : expr =
  match (s1, s2) with
  | Set l1, Set l2 -> remove_duplicates (Ast.Set (l1 @ l2))
  | _ -> raise (InvalidSet "Not a set")

let rec intersect_helper (l : expr list) (s : expr) =
  match l with
  | h :: t ->
      if is_in_boolean h s then h :: intersect_helper t s
      else intersect_helper t s
  | [] -> []

let intersect (s1 : expr) (s2 : expr) : expr =
  match s1 with
  | Set l -> Ast.Set (intersect_helper l s2)
  | _ -> raise (InvalidSet "Not a set")

(* makes sure every element of l1 is in l2*)
let rec is_subset_helper (l1 : expr list) (l2 : expr list) =
  match l1 with
  | [] -> true
  | h :: t -> if List.mem h l2 then is_subset_helper t l2 else false

let is_subset (s1 : expr) (s2 : expr) : bool =
  match (s1, s2) with
  | Set l1, Set l2 -> is_subset_helper l1 l2
  | _ -> raise (InvalidSet "Not a set")

let equals (s1 : expr) (s2 : expr) : bool = is_subset s1 s2 && is_subset s2 s1
