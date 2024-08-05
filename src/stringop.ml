(** Operations on strings *)

(** mult_string s t repeats a (string s) t times Author: Kabir Samsi (kas499)

    @param s holds the starting string
    @param t stores the number of times to repeat the string
    @return - repeated concatenated string *)
let rec mult_string (s : string) (t : int) =
  match t with
  | 0 -> ""
  (*Base case (empty string)*)
  (* Repeat string recursively and decrement counter *)
  | i -> s ^ mult_string s (i - 1)

(*rev_list s reverses a list *)
let rec rev_list (s : 'a list) =
  match s with
  | [] -> []
  | h :: t -> rev_list t @ [ h ]

(** merge_string s takes a charlist and merges it into a string Author: Kabir
    Samsi (kas499)

    @param s holds the char list
    @return - Merged string *)
let rec merge_string (s : char list) =
  match s with
  | [] -> ""
  (*Base case - empty string*)
  (* Form string with current character and concat with merged remainder*)
  | h :: t -> String.make 1 h ^ merge_string t

(** to_char_array s takes a string and splits it into characters Author: Kabir
    Samsi (kas499)

    @param s holds the string
    @return - Split string list *)
let rec to_char_list (s : string) =
  match s with
  | "" -> []
  (*Base case - empty string*)
  (* Extract first character and cons with char array of remaining string*)
  | _ -> String.get s 0 :: to_char_list (String.sub s 1 (String.length s - 1))

(*rev_string s reverses s*)
let rev_string (s : string) = s |> to_char_list |> rev_list |> merge_string
