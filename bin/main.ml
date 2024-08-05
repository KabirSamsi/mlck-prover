open Logic
open Evalast

(* REPL *)

type state = { cmd : string }

let initial_st = { cmd = "" }
let eol_regex = Str.regexp {|^.*;;|}

let matches s r =
  if Str.string_match r s 0 then Some (Str.matched_string s) else None

let rec loop state =
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "#quit;;" -> Stdlib.exit 0
  | str -> (
      match matches str eol_regex with
      | None ->
          let state' = { cmd = state.cmd ^ str } in
          loop state'
      | Some last ->
          (let last_line = String.sub str 0 (String.length str - 2) in
           let cmd = state.cmd ^ last_line in
           let state = { cmd = "" } in
           try
             print_endline (Evalast.interp cmd);
             print_newline ();
             loop state
           with Lexer.LexingError s ->
             print_endline "Bad statement. Enter something else:");
          loop state)

(** [main ()] prompts for an expression to be evaluated. *)
let main () =
  print_newline ();
  print_endline "REPL";
  print_newline ();
  print_endline "Enter a statement:";
  loop initial_st

(* Run the engine. *)
let () = main ()
