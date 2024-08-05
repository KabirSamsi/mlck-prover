open OUnit2
open Logic
open Ast
open Sets
open Evalast

(* TESTING APPROACH : Below is our test suite for our project. We tested
   Evalast.ml and Sets.ml using OUnit, as well as running some additional
   manual tests for all expressions using the repl. We used a mix of black-box
   and glass-box testing, specifically using Bisect to help us achieve higher
   coverage glass-box testing (with both Evalast and Sets). Our manual testing
   partially utilized black-box testing as we entered data based on the
   specifications of our system's language. The high coverage we achieved in
   combination with the testing done in the repl gives us confidence that our
   system is working as intended. Furthermore, our test approach demonstrates
   the correctness of our system, as our coverage shows testing of all possible
   promptings of the system, both in the form of expressions and entered text.  
*)

(* TEST SUITE for Evaluation and Lexing  *)

let one = Integer 1
let two = Integer 2
let three = Integer 3

(*Test the at operation on expr lists*)
let at_test (name : string) (ls : expr list) (search : int) (expected : string)
    : test =
  name >:: fun _ -> assert_equal expected (at ls search |> to_string)

(*Test to_string on any given expression*)
let to_string_test (name : string) (e : expr) (expected : string) : test =
  name >:: fun _ -> assert_equal expected (e |> to_string)

(*Test any binary operator application onto two expressions*)
let bop_test (name : string) f (e1 : expr) (e2 : expr) (expected : string) :
    test =
  name >:: fun _ -> assert_equal expected (f e1 e2 |> to_string)

(*Successfully test any unary operator application onto two expressions*)
let uop_test (name : string) f (e : expr) (expected : expr) : test =
  name >:: fun _ -> assert_equal expected (f e)

(*Tests that any two expressions are successfully evaluated*)
let eval_test (name : string) (e : expr) (expected : string) : test =
  name >:: fun _ -> assert_equal expected (e |> eval |> to_string)

(*Tests that any invalid expressions successfully raise an error *)
let eval_failure (name : string) (e : expr) (expected : string) : test =
  name >:: fun _ -> assert_raises (Incompatible expected) (fun () -> eval e)

(*Tests that any invalid expressions for sets successfully raise an error *)
let set_failure (name : string) (e : expr) (expected : string) : test =
  name >:: fun _ -> assert_raises (Sets.InvalidSet expected) (fun () -> eval e)

(*Tests that the equality checker function works on two expressions*)
let eq_test (name : string) (e1 : expr) (e2 : expr) (expected : string) : test =
  name >:: fun _ -> assert_equal expected (eq_eval e1 e2 |> to_string)

(*Tests that the an equality fails*)
let eq_failure (name : string) (e1 : expr) (e2 : expr) (expected : string) :
    test =
  name >:: fun _ ->
  assert_raises (Incompatible expected) (fun () -> eq_eval e1 e2)

(* Tests that a string is correctly parsed, evaluated, then converted to a
   string *)
let interp_test (name : string) (s : string) (expected : string) : test =
  name >:: fun _ -> assert_equal expected (interp s) ~printer:Fun.id

let eval_tests =
  [
    (*Eval for binary operators*)
    eval_test "Evaluating 2 + 3" (BopExpr (Add, two, three)) "5";
    eval_test "Evaluating 10 - 20" (BopExpr (Sub, Integer 10, Integer 20)) "-10";
    eval_test "Evaluating 10 - 2.5" (BopExpr (Sub, Integer 10, Float 2.5)) "7.5";
    eval_test "Evaluating 10.5 - 2" (BopExpr (Sub, Float 10.5, two)) "8.5";
    eval_test "Subtracting 't' from 'potatoes'"
      (BopExpr (Sub, Str "potatoes", Character 't'))
      "\"poaoes\"";
    eval_test "Subtracting 3 from 'potatoes'"
      (BopExpr (Sub, Str "potatoes", three))
      "\"potat\"";
    eval_test "Subtracting 't' from 'potatoes'"
      (BopExpr (Sub, Str "potatoes", Character 't'))
      "\"poaoes\"";
    eval_test "Evaluating 2 + 4 * 2"
      (BopExpr (Add, two, BopExpr (Mult, Integer 4, two)))
      "10";
    eval_test "Evaluating 2 + 5.5 = 7.5"
      (BopExpr (Eq, BopExpr (Add, two, Float 5.5), Float 7.5))
      "true";
    eval_test "Evaluating true = true"
      (BopExpr (Eq, Boolean true, Boolean true))
      "true";
    eval_test "Evaluating 'c' ≠ 'a'"
      (BopExpr (Eq, Character 'c', Character 'a'))
      "false";
    eval_test "Evaluating that 5 < 10"
      (BopExpr (Lt, Integer 5, Integer 10))
      "true";
    eval_test "Evaluating that 5 ≤ 10"
      (BopExpr (Leq, Integer 5, Integer 10))
      "true";
    eval_test "Evaluating that 'hello' * 5 = 'hellohellohellohellohello'"
      (BopExpr
         ( Eq,
           BopExpr (Mult, Str "hello", Integer 5),
           Str "hellohellohellohellohello" ))
      "true";
    eval_test "Evaluating that (10 + 7) = 17"
      (BopExpr (Eq, BopExpr (Add, Integer 10, Integer 7), Integer 17))
      "true";
    (*Should evaluate to true (checks that string is split incorrectly)*)
    eval_test
      "Evaluating dividing 'ocaml syntax' by 'a' is not ['ocaml'; 'syntax']"
      (BopExpr
         ( Neq,
           BopExpr (Div, Str "ocaml syntax", Character 'a'),
           Set [ Str "ocaml"; Str "syntax" ] ))
      "true";
    (*Should evaluate to true (checks that string is split correctly)*)
    eval_test
      "Evaluating that dividing \n\
      \    'ocaml syntax' by 'a' is  ['oc'; 'ml synt'; 'x']"
      (BopExpr
         ( Eq,
           BopExpr (Div, Str "ocaml syntax", Character 'a'),
           Set [ Str "oc"; Str "ml synt"; Str "x" ] ))
      "true";
    eval_test "Evaluating that 'oodbye' ≥ 'hello'"
      (BopExpr (Geq, BopExpr (Sub, Str "goodbye", Character 'g'), Str "hello"))
      "true";
    eval_test "Evaluating that 2.3 > 1.5"
      (BopExpr (Gt, Float 2.3, Float 1.5))
      "true";
    eval_test "Evaluating that true > false"
      (BopExpr (Gt, Boolean true, Boolean false))
      "true";
    eval_test "Evaluating that 'a' > 'c'"
      (BopExpr (Gt, Character 'a', Character 'c'))
      "false";
    eval_test "Evaluating summing an integer with None"
      (BopExpr (Add, None, three))
      "3";
    eval_test "Evaluating summing a vector with None"
      (BopExpr (Add, None, Vector [ one; two; three ]))
      "< 1 2 3 >";
    eval_test "Evaluating that 2+3 = 5 and 5+5 = 10"
      (BopExpr
         ( And,
           BopExpr (Eq, BopExpr (Add, two, three), Integer 5),
           BopExpr (Eq, BopExpr (Add, Integer 5, Integer 5), Integer 10) ))
      "true";
    eval_test "Evaluating inverse of a boolean"
      (UnExpr (Inv, Boolean true))
      "false";
    eval_test "Evaluating that 2+3 = 5 or 5+5 = 11"
      (BopExpr
         ( Or,
           BopExpr (Eq, BopExpr (Add, two, three), Integer 5),
           BopExpr (Eq, BopExpr (Add, Integer 5, Integer 5), Integer 11) ))
      "true";
    eval_test "Evaluating 2+3 = 5 xor 5+5 = 10"
      (BopExpr
         ( Xor,
           BopExpr (Eq, BopExpr (Add, two, three), Integer 5),
           BopExpr (Eq, BopExpr (Add, Integer 5, Integer 5), Integer 10) ))
      "false";
    eval_test "Evaluating summing a vector with None"
      (Vector [ one; two ])
      "< 1 2 >";
    eval_test "Evaluating None" None "<none>";
    eval_test "Evaluating summing a float with None"
      (BopExpr (Add, None, Float 3.0))
      "3.";
    eval_test "Empty set" (Set []) "{ }";
    eval_test "Creating set" (Set [ one; two ]) "{ 1 2 }";
    eval_test "Creating set with duplicate" (Set [ one; one; two ]) "{ 1 2 }";
    eval_test "Creating set with multiple duplicates"
      (Set [ one; one; two; two; one; one; two; two ])
      "{ 1 2 }";
    eval_test "Adding to set" (BopExpr (Setadd, one, Set [ two ])) "{ 1 2 }";
    eval_test "Adding duplicate to set"
      (BopExpr (Setadd, one, Set [ one; two ]))
      "{ 1 2 }";
    eval_test "Removing from set"
      (BopExpr (Setremove, two, Set [ one; two ]))
      "{ 1 }";
    eval_test "Removing from set with duplicate"
      (BopExpr (Setremove, two, Set [ one; two; two; two ]))
      "{ 1 }";
    eval_test "Removing from empty set is empty"
      (BopExpr (Setremove, two, Set []))
      "{ }";
    eval_test "Set union"
      (BopExpr (Union, Set [ one ], Set [ one; two ]))
      "{ 1 2 }";
    eval_test "Set union with empty set"
      (BopExpr (Union, Set [], Set [ one; two ]))
      "{ 1 2 }";
    eval_test "Set intersection"
      (BopExpr (Intersection, Set [ one ], Set [ one; two ]))
      "{ 1 }";
    eval_test "Set intersection with empty set"
      (BopExpr (Intersection, Set [], Set [ one; two ]))
      "{ }";
    eval_test "Nonempty subset"
      (BopExpr (Subset, Set [ one ], Set [ one; two ]))
      "true";
    eval_test "One empty subset"
      (BopExpr (Subset, Set [], Set [ one; two ]))
      "true";
    eval_test "Not a subset"
      (BopExpr (Subset, Set [ one; two ], Set [ one ]))
      "false";
    eval_test "Both empty subsets" (BopExpr (Subset, Set [], Set [])) "true";
    eval_test "Empty sets are equal"
      (BopExpr (Setequals, Set [], Set []))
      "true";
    eval_test "Nonempty sets are equal"
      (BopExpr (Setequals, Set [ one; two ], Set [ one; two ]))
      "true";
    eval_test "Nonempty sets are not equal"
      (BopExpr (Setequals, Set [ one ], Set [ one; two ]))
      "false";
    eval_test "Empty set and nonempty set are not equal"
      (BopExpr (Setequals, Set [], Set [ one ]))
      "false";
    eval_test "Nonempty set and empty set are not equal"
      (BopExpr (Setequals, Set [ one ], Set []))
      "false";
    eval_failure "Evaluating that 2 > 'test' causes an error."
      (BopExpr (Gt, two, Str "test"))
      "Incompatible with integer";
    eval_failure "Evaluating that 2 - 'test' causes an error."
      (BopExpr (Sub, two, Str "test"))
      "Incompatible types";
    eval_failure "Evaluating that 2.5 > 'test' causes an error."
      (BopExpr (Gt, Float 2.5, Str "test"))
      "Incompatible with float";
    eval_failure "Evaluating that true > 'test' causes an error."
      (BopExpr (Gt, Boolean true, Str "test"))
      "Incompatible with boolean";
    eval_failure "Evaluating that 'hello' > 5 causes an error."
      (BopExpr (Gt, Str "hello", Integer 5))
      "Incompatible with string";
    eval_failure "Evaluating that 'h' > 5 causes an error."
      (BopExpr (Gt, Character 'h', Integer 5))
      "Incompatible with char";
    eval_failure "Evaluating that vector > vector causes an error."
      (BopExpr (Gt, Vector [ one ], Vector [ two ]))
      "Incompatible types";
    eval_failure "Evaluating that adding an integer and string causes an error."
      (BopExpr (Add, two, Str "test"))
      "Incompatible types";
    eval_failure "Evaluating that adding a float and string causes an error."
      (BopExpr (Add, Float 2., Str "test"))
      "Incompatible types";
    eval_failure
      "Evaluating that subtracting a string from float causes an error."
      (BopExpr (Sub, Float 2., Str "test"))
      "Incompatible types";
    eval_failure
      "Evaluating that subtracting a float from string causes an error."
      (BopExpr (Sub, Str "test", Float 2.))
      "Incompatible types";
    eval_failure "Evaluating that adding a vector and integer causes an error."
      (BopExpr (Add, Vector [ one; two ], two))
      "Incompatible types";
    eval_failure
      "Evaluating that subtracting an int from a vector causes an error."
      (BopExpr (Sub, Vector [ one; two ], two))
      "Incompatible types";
    eval_failure "Evaluating that adding a character and int causes an error."
      (BopExpr (Add, Character 'c', two))
      "Incompatible types";
    eval_failure "Evaluating that adding two sets causes an error."
      (BopExpr (Add, Set [ one; two ], Set [ one; two ]))
      "Incompatible types";
    eval_failure
      "Evaluating that subtracting an int from a bool causes an error."
      (BopExpr (Sub, Boolean false, two))
      "Incompatible types";
    eval_failure "Evaluating that dividing an int by a string causes an error."
      (BopExpr (Div, two, Str "hello"))
      "Incompatible types";
    eval_failure "Evaluating that dividing a float by a string causes an error."
      (BopExpr (Div, Float 2., Str "hello"))
      "Incompatible types";
    eval_failure "Evaluating that dividing a string by an int causes an error."
      (BopExpr (Div, Str "hello", two))
      "Incompatible types";
    eval_failure
      "Evaluating that dividing a vector by a boolean causes an error."
      (BopExpr (Div, Vector [ one; two; three ], Boolean false))
      "Incompatible types";
    eval_failure
      "Evaluating that dividing a boolean by a boolean causes an error."
      (BopExpr (Div, Boolean true, Boolean false))
      "Incompatible types";
    eval_failure "Evaluating that adding a string and float causes an error."
      (BopExpr (Add, Str "test", Float 2.0))
      "Incompatible types";
    eval_failure "Evaluating that multiplying an int and bool causes an error"
      (BopExpr (Mult, two, Boolean false))
      "Incompatible types";
    eval_failure "Evaluating that multiplying a float and bool causes an error"
      (BopExpr (Mult, Float 2.5, Boolean false))
      "Incompatible types";
    eval_failure "Evaluating that multiplying a string and bool causes an error"
      (BopExpr (Mult, Str "hello", Boolean false))
      "Incompatible types";
    eval_failure "Evaluating that multiplying a vector and bool causes an error"
      (BopExpr (Mult, Vector [ one; two ], Boolean false))
      "Incompatible types";
    eval_failure "Evaluating that multiplying a bool and int causes an error"
      (BopExpr (Mult, Boolean false, Integer 12))
      "Incompatible types";
    (*Unary operators*)
    eval_failure "Evaluating that negating an integer sum causes an error"
      (UnExpr (Not, BopExpr (Add, two, three)))
      "Cannot evaluate non-bool expression";
    eval_failure "Evaluating that int and bool is not valid"
      (BopExpr (And, three, Boolean false))
      "Incompatible types";
    eval_failure "Evaluating that bool and int is not valid"
      (BopExpr (And, Boolean false, three))
      "Incompatible types";
    eval_failure "Evaluating that int or bool is not valid"
      (BopExpr (Or, three, Boolean false))
      "Incompatible types";
    eval_failure "Evaluating that bool or int is not valid"
      (BopExpr (Or, Boolean false, three))
      "Incompatible types";
    eval_failure "Evaluating that bool xor int is not valid"
      (BopExpr (Xor, Boolean true, one))
      "Incompatible types";
    eval_failure "Evaluating that int xor int is not valid"
      (BopExpr (Xor, two, one))
      "Incompatible types";
    eval_failure
      "Evaluating that crossing vectors of incorrect dimensions fails"
      (BopExpr (Cross, Vector [ one; two ], Vector [ three ]))
      "Vectors must both be of length 3";
    eval_failure "Evaluating inverse on a character"
      (UnExpr (Inv, Character 'c'))
      "No inverse exists";
    eval_failure "Evaluating that crossing non-vectors fails"
      (BopExpr (Cross, two, three))
      "Incompatible types";
    (* set_failure "is_in not a set" (Sets.is_in one one) "Not a set"; *)
    eval_test "Evaluating the inverse \'hello\' is \'olleh\'"
      (UnExpr (Inv, Str "hello"))
      "\"olleh\"";
    bop_test "Adding two integers" add one two "3";
    bop_test "Adding two floats" add (Float 1.) (Float 2.) "3.";
    bop_test "Adding two strings" add (Str "O") (Str "caml") "\"Ocaml\"";
    bop_test "Adding integer and float" add one (Float 2.) "3.";
    bop_test "Adding float and integer" add (Float 1.) two "3.";
    bop_test "Adding string and character" add (Str "Ocam") (Character 'l')
      "\"Ocaml\"";
    bop_test "Adding character and string" add (Character 'O') (Str "caml")
      "\"Ocaml\"";
    bop_test "Adding two characters" add (Character 'O') (Character 'K')
      "\"OK\"";
    bop_test "Adding character with none" add (Character 'O') None "'O'";
    bop_test "Multiplying two integers" mult two three "6";
    bop_test "Multiplying two floats" mult (Float 2.) (Float 3.) "6.";
    bop_test "Multiplying string once" mult (Str "hi") one "\"hi\"";
    bop_test "Multiplying string twice" mult (Str "hi") two "\"hihi\"";
    bop_test "Multiplying integer and float" mult two (Float 3.) "6.";
    bop_test "Multiplying float and integer" mult (Float 2.) three "6.";
    set_failure "Setadd raises InvalidSet"
      (BopExpr (Setadd, one, two))
      "Not a set";
    set_failure "Setremove raises InvalidSet"
      (BopExpr (Setremove, one, two))
      "Not a set";
    set_failure "Union raises InvalidSet"
      (BopExpr (Union, one, two))
      "Not a set";
    set_failure "Intersection raises InvalidSet"
      (BopExpr (Intersection, one, two))
      "Not a set";
    set_failure "Subset raises InvalidSet"
      (BopExpr (Subset, one, two))
      "Not a set";
    set_failure "Setequals raises InvalidSet"
      (BopExpr (Setequals, one, two))
      "Not a set";
  ]

(** [parse_test name expected_output str] creates an OUnit test named [name]
    that asserts the equalitiy of [expected_output] and [parse str]. *)
let parse_test name expected_output str =
  name >:: fun _ -> assert_equal expected_output (parse str)

let parse_tests =
  [
    parse_test "parse 2" two "2";
    parse_test "parse 2." (Float 2.) "2.";
    parse_test "parse .2" (Float 0.2) ".2";
    parse_test "parse 22.2" (Float 22.2) "22.2";
    parse_test "parse true" (Boolean true) "true";
    parse_test "parse false" (Boolean false) "false";
    parse_test "parse hello" (Str "hello") "\"hello\"";
    parse_test "parse !true" (UnExpr (Not, Boolean true)) "!true";
    parse_test "parse !2" (UnExpr (Not, two)) "!2";
    parse_test "parse 2+4" (BopExpr (Add, two, Integer 4)) "2+4";
    parse_test "parse 2-4" (BopExpr (Sub, two, Integer 4)) "2-4";
    parse_test "parse 2*4" (BopExpr (Mult, two, Integer 4)) "2*4";
    parse_test "parse 2/4" (BopExpr (Div, two, Integer 4)) "2/4";
    parse_test "parse true || false"
      (BopExpr (Or, Boolean true, Boolean false))
      "true || false";
    parse_test "parse true && false"
      (BopExpr (And, Boolean true, Boolean false))
      "true && false";
    parse_test "parse true XOR false"
      (BopExpr (Xor, Boolean true, Boolean false))
      "true XOR false";
    parse_test "parse 1 == 2" (BopExpr (Eq, one, two)) "1 == 2";
    (* parse_test "parse 1 != 2" (BopExpr (Eq, one, two)) "1 != 2"; *)
    parse_test "parse 1 > 2" (BopExpr (Gt, one, two)) " 1 > 2";
    parse_test "parse 1 < 2" (BopExpr (Lt, one, two)) " 1 < 2";
    parse_test "parse 1 >= 2" (BopExpr (Geq, one, two)) " 1 >= 2";
    parse_test "parse 1 <= 2" (BopExpr (Leq, one, two)) " 1 <= 2";
  ]

let vector_tests =
  [
    (*Vector addition tests*)
    bop_test "Adding two int vectors" add
      (Vector [ one; two; three ])
      (Vector [ Integer 10; Integer 12; Integer 23 ])
      "< 11 14 26 >";
    bop_test "Adding a vector with subexpressions" add
      (Vector
         [ add one two; sub (Integer 4) two; mult (Integer 15) (Integer 5) ])
      (Vector [ Integer 10; Integer 12; Integer 23 ])
      "< 13 14 98 >";
    bop_test "Adding two string vectors" add
      (Vector [ Str "hello"; Str "foo"; Str "functional" ])
      (Vector [ Str " world"; Str " bar"; Str " programming" ])
      "< \"hello world\" \"foo bar\" \"functional programming\" >";
    bop_test "Adding two float vectors" add
      (Vector [ Float 1.5; Float 2.3; Float 3.8 ])
      (Vector [ Float 12.3; Float 14.4; Float 15.5 ])
      "< 13.8 16.7 19.3 >";
    (*Vector multiplication tests*)
    bop_test "Multiplying scalar integer with int vector" mult (Integer 5)
      (Vector [ one; two; three ])
      "< 5 10 15 >";
    bop_test "Multiplying scalar integer with float vector" mult (Integer 5)
      (Vector [ Float 1.5; Float 2.5; Float 13.5 ])
      "< 7.5 12.5 67.5 >";
    bop_test "Multiplying scalar float with int vector" mult (Float 5.2)
      (Vector [ one; two; three ])
      "< 5.2 10.4 15.6 >";
    bop_test "Multiplying string with int vector" mult (Str "hello")
      (Vector [ one; two; three ])
      "< \"hello\" \"hellohello\" \"hellohellohello\" >";
    bop_test "Dot product of two integer vectors" mult
      (Vector [ one; two; three ])
      (Vector [ Integer 4; Integer 10; Integer 9 ])
      "51";
    bop_test "Dot product of character and integer vectors" mult
      (Vector [ Character 'h'; Character 'f'; Character 'g' ])
      (Vector [ Integer 4; Integer 5; two ])
      "\"hhhhfffffgg\"";
    (*Vector subtraction tests*)
    bop_test "Subtracting two int vectors" sub
      (Vector [ Integer 25; Integer 27; Integer 34 ])
      (Vector [ Integer 10; Integer 12; Integer 23 ])
      "< 15 15 11 >";
    bop_test "Subtracting char and string vectors" sub
      (Vector [ Str "hello"; Str "world"; Str "goodbye" ])
      (Vector [ Character 'l'; Character 'r'; Character 'o' ])
      "< \"heo\" \"wold\" \"gdbye\" >";
    bop_test "Adding two float vectors" add
      (Vector [ Float 1.5; Float 2.3; Float 3.8 ])
      (Vector [ Float 12.3; Float 14.4; Float 15.5 ])
      "< 13.8 16.7 19.3 >";
    (*Vector division tests*)
    bop_test "Dividing an integer vector by an integer" div
      (Vector [ Integer 10; Integer 15 ])
      (Integer 5) "< 2 3 >";
    bop_test "Dividing an integer vector by a float" div
      (Vector [ Integer 10; Integer 15 ])
      (Float 2.5) "< 4. 6. >";
    bop_test "Dividing a float vector by an integer" div
      (Vector [ Float 10.6; Float 14.8 ])
      two "< 5.3 7.4 >";
    bop_test "Dividing a float vector by a float" div
      (Vector [ Float 10.6; Float 14.8 ])
      (Float 2.) "< 5.3 7.4 >";
    bop_test "Dividing a string vector by a given character" div
      (Vector [ Str "hello"; Str "world" ])
      (Character 'l') "< { \"he\" \"\" \"o\" } { \"wor\" \"d\" } >";
    (*Vector cross product tests*)
    bop_test "Cross product of two integer vectors" cross
      (Vector [ one; two; three ])
      (Vector [ Integer 4; Integer 10; Integer 9 ])
      "< -12 3 2 >";
    bop_test "Cross product of float vectors" cross
      (Vector [ one; two; three ])
      (Vector [ Float 4.0; Float 10.0; Float 9.0 ])
      "< -12. 3. 2. >";
    bop_test "Equality of two int vectors" eq_eval
      (Vector [ one; two; three ])
      (Vector [ Integer 2; Integer 4; Integer 6 ])
      "true";
    bop_test "Equality of an int and float vector" eq_eval
      (Vector [ one; two; three ])
      (Vector [ Float 3.5; Float 7.0; Float 10.5 ])
      "true";
    bop_test "Inequality of two vectors" eq_eval
      (Vector [ one; two; three ])
      (Vector [ Integer 2; Integer 5; Integer 6 ])
      "false";
  ]

let set_tests =
  [
    ("Empty set constructor" >:: fun _ -> assert_equal (Sets.empty ()) (Set []));
    ( "Empty set is empty" >:: fun _ ->
      assert_equal true (Sets.is_empty (Sets.empty ())) );
    ( "Adding one is not empty" >:: fun _ ->
      assert_equal false (Sets.is_empty (Sets.add one (Sets.empty ()))) );
    ( "Adding multiple times only adds one" >:: fun _ ->
      assert_equal
        (Sets.add one (Sets.add one (Sets.empty ())))
        (Sets.add one (Sets.empty ())) );
    ( "Adding and removing yields empty" >:: fun _ ->
      assert_equal (Sets.empty ())
        (Sets.remove one (Sets.add one (Sets.empty ()))) );
    ( "Adding and removing yields original set" >:: fun _ ->
      assert_equal
        (Sets.add two (Sets.empty ()))
        (Sets.remove one (Sets.add two (Sets.add one (Sets.empty ())))) );
    ( "Union without repeat" >:: fun _ ->
      assert_equal
        (Sets.add two (Sets.add one (Sets.empty ())))
        (Sets.union
           (Sets.add two (Sets.empty ()))
           (Sets.add one (Sets.empty ()))) );
    ( "Union with repeat" >:: fun _ ->
      assert_equal
        (Sets.add two (Sets.add one (Sets.empty ())))
        (Sets.union
           (Sets.add two (Sets.empty ()))
           (Sets.add two (Sets.add one (Sets.empty ())))) );
    ( "Union with empty set" >:: fun _ ->
      assert_equal
        (Sets.add two (Sets.add one (Sets.empty ())))
        (Sets.union (Sets.empty ())
           (Sets.add two (Sets.add one (Sets.empty ())))) );
    ( "Union of identical sets is the same set" >:: fun _ ->
      assert_equal
        (Sets.add two (Sets.add one (Sets.empty ())))
        (Sets.union
           (Sets.add two (Sets.add one (Sets.empty ())))
           (Sets.add two (Sets.add one (Sets.empty ())))) );
    ( "Intersect of disjoint sets is empty" >:: fun _ ->
      assert_equal (Sets.empty ())
        (Sets.intersect
           (Sets.add one (Sets.empty ()))
           (Sets.add two (Sets.empty ()))) );
    ( "Intersect with some overlap" >:: fun _ ->
      assert_equal
        (Sets.add two (Sets.empty ()))
        (Sets.intersect
           (Sets.add two (Sets.add one (Sets.empty ())))
           (Sets.add two (Sets.empty ()))) );
    ( "Intersect with empty set" >:: fun _ ->
      assert_equal (Sets.empty ())
        (Sets.intersect
           (Sets.add two (Sets.add one (Sets.empty ())))
           (Sets.empty ())) );
    ( "Intersect of identical sets is the same set" >:: fun _ ->
      assert_equal
        (Sets.add two (Sets.add one (Sets.empty ())))
        (Sets.intersect
           (Sets.add two (Sets.add one (Sets.empty ())))
           (Sets.add two (Sets.add one (Sets.empty ())))) );
    ( "Subset test" >:: fun _ ->
      assert_equal true
        (Sets.is_subset
           (Sets.add one (Sets.empty ()))
           (Sets.add one (Sets.empty ()))) );
    ( "Subset test with empty set" >:: fun _ ->
      assert_equal true
        (Sets.is_subset (Sets.empty ()) (Sets.add one (Sets.empty ()))) );
    ( "Subset test both empty sets" >:: fun _ ->
      assert_equal true (Sets.is_subset (Sets.empty ()) (Sets.empty ())) );
    ( "Not a Subset test" >:: fun _ ->
      assert_equal false
        (Sets.is_subset (Sets.add one (Sets.empty ())) (Sets.empty ())) );
    ( "Equals test" >:: fun _ ->
      assert_equal true
        (Sets.equals
           (Sets.add one (Sets.empty ()))
           (Sets.add one (Sets.empty ()))) );
    ( "Equals test empty" >:: fun _ ->
      assert_equal true (Sets.equals (Sets.empty ()) (Sets.empty ())) );
    ( "Not equals test" >:: fun _ ->
      assert_equal false
        (Sets.equals
           (Sets.add two (Sets.add one (Sets.empty ())))
           (Sets.add one (Sets.empty ()))) );
    ( "Not equals test empty" >:: fun _ ->
      assert_equal false
        (Sets.equals (Sets.empty ()) (Sets.add one (Sets.empty ()))) );
  ]

let at_tests =
  [
    at_test "Evaluating empty list with invalid index" [] 3 "<none>";
    at_test "Evaluating empty list" [] ~-1 "<none>";
  ]

let to_string_tests =
  [
    to_string_test "Testing negation expression"
      (UnExpr (Not, Boolean true))
      "!true";
    to_string_test "Testing inverse expression"
      (UnExpr (Inv, three))
      "Inverse of 3";
    to_string_test "Testing addition expression"
      (BopExpr (Add, three, two))
      "3 + 2";
    to_string_test "Testing subtraction expression"
      (BopExpr (Sub, three, two))
      "3 - 2";
    to_string_test "Testing multiplication expression"
      (BopExpr (Mult, three, two))
      "3 * 2";
    to_string_test "Testing division expression"
      (BopExpr (Div, three, two))
      "3 / 2";
    to_string_test "Testing and expression"
      (BopExpr (And, Boolean true, Boolean false))
      "true ʌ false";
    to_string_test "Testing or expression"
      (BopExpr (Or, Boolean true, Boolean false))
      "true v false";
    to_string_test "Testing xor expression"
      (BopExpr (Xor, Boolean true, Boolean false))
      "true ⊕ false";
    to_string_test "Testing equal expression"
      (BopExpr (Eq, Float 2.3, Float 2.3))
      "2.3 = 2.3";
    to_string_test "Testing inequal expression"
      (BopExpr (Neq, Float 2.3, Float 4.5))
      "2.3 ≠ 4.5";
    to_string_test "Testing gt expression"
      (BopExpr (Gt, Float 4.5, Float 2.3))
      "4.5 > 2.3";
    to_string_test "Testing lt expression"
      (BopExpr (Lt, Float 2.3, Float 4.5))
      "2.3 < 4.5";
    to_string_test "Testing gte expression"
      (BopExpr (Geq, Float 4.5, Float 2.3))
      "4.5 ≥ 2.3";
    to_string_test "Testing lte expression"
      (BopExpr (Leq, Float 2.3, Float 4.5))
      "2.3 ≤ 4.5";
    to_string_test "Testing cross product expression"
      (BopExpr
         ( Cross,
           Vector [ one; two; three ],
           Vector [ Integer 4; Integer 5; Integer 6 ] ))
      "< 1 2 3 > x < 4 5 6 >";
  ]

let eq_tests =
  [
    eq_test "Testing equality of nones" None None "true";
    eq_test "Testing equality of none and other" None two "false";
    eq_test "Testing equality of integers" two two "true";
    eq_test "Testing inequality of integers" two three "false";
    eq_test "Testing equality of float and integer" (Float 2.) two "true";
    eq_test "Testing equality of character and string" (Character 'h') (Str "h")
      "true";
    eq_test "Testing equality of vectors"
      (Vector [ one; two; three ])
      (Vector [ one; two; three ])
      "true";
    eq_failure "Testing failed equality of integer and string" two (Str "hello")
      "Incompatible with integer";
    eq_failure "Testing failed equality of float and string" (Float 2.0)
      (Str "hello") "Incompatible with float";
    eq_failure "Testing failed equality of boolean and string" (Boolean true)
      (Str "hello") "Incompatible with boolean";
    eq_failure "Testing failed equality of string and int" (Str "hello") two
      "Incompatible with string";
    eq_failure "Testing failed equality of character and int" (Character 'h')
      two "Incompatible with char";
    eq_failure "Testing failed equality of vector and int" (Vector []) two
      "Incompatible with vector";
    eq_failure "Testing failed equality of set and int" (Set []) two
      "Incompatible with set";
  ]

let interp_tests =
  [
    interp_test "interp int" "2" "2";
    interp_test "interp negative int" "(-2)" "-2";
    interp_test "interp float" "1.2" "1.2";
    interp_test "interp negative float" "(-1.2)" "-1.2";
    interp_test "interp float starting with decimal" ".2" "0.2";
    interp_test "interp negative float starting with decimal" "(-.2)" "-0.2";
    interp_test "interp true" "true" "true";
    interp_test "interp false" "false" "false";
    interp_test "interp char" "'a'" "'a'";
    interp_test "interp string" {|"hello world"|} {|"hello world"|};
    interp_test "interp bop add" "2 + 2" "4";
    interp_test "interp bop sub" "2 - 2" "0";
    interp_test "interp bop mul" " 2 * 2" "4";
    interp_test "interp bop div" "2 / 2" "1";
    interp_test "interp bop > true" "2 > 1" "true";
    interp_test "interp bop > false" "0 > 1" "false";
    interp_test "interp bop >= true" "2 >= 2" "true";
    interp_test "interp bop >= false" "0 >= 1" "false";
    interp_test "interp bop < false" "2 < 1" "false";
    interp_test "interp bop < true" "0 < 1" "true";
    interp_test "interp bop <= true" "2 <= 2" "true";
    interp_test "interp bop <= false" "1 <= 0" "false";
    interp_test "interp set" "{1, 2, 3}" "{ 1 2 3 }";
    interp_test "interp set union" "{1, 2, 3} {U} {3, 4, 5}" "{ 1 2 3 4 5 }";
    interp_test "interp set union empty" "{1, 2, 3} {U} {}" "{ 1 2 3 }";
    interp_test "interp set intersection" "{1, 2, 3} {I} {3, 4, 5}" "{ 3 }";
    interp_test "interp set intersection empty" "{1, 2, 3} {I} {}" "{ }";
    interp_test "interp set add" "3 {+} {1, 2, 3}" "{ 1 2 3 }";
    interp_test "interp set add new element" "4 {+} {1, 2, 3}" "{ 4 1 2 3 }";
    interp_test "interp set add multiple" "4 {+} (3 {+} {1, 2})" "{ 4 3 1 2 }";
    interp_test "interp set remove" "1 {-} {1, 2, 3}" "{ 2 3 }";
    interp_test "interp set remove non-element" "4 {-} {1, 2, 3}" "{ 1 2 3 }";
    interp_test "interp set subset of self" "{1, 2, 3} {S} {1, 2, 3}" "true";
    interp_test "interp set subset true" "{1, 3} {S} {1, 2, 3}" "true";
    interp_test "interp set subset true empty" "{} {S} {1, 2, 3}" "true";
    interp_test "interp set subset false" "{1, 2, 3} {S} {1, 3}" "false";
    interp_test "interp set equals true empty" "{} {=} {}" "true";
    interp_test "interp set equals true" "{1, 2, 3} {=} {1, 2, 3}" "true";
    interp_test "interp set equals false" "{1, 2, 3} {=} {1, 3}" "false";
    interp_test "interp vector" "<:1, 2, 3:>" "< 1 2 3 >";
    interp_test "interp vector add" "<:1, 2, 3:> + <:2, 3, 4:>" "< 3 5 7 >";
    interp_test "interp vector sub" "<:1, 2, 3:> - <:2, 3, 4:>" "< -1 -1 -1 >";
    interp_test "interp vector mult" "<:1, 2, 3:> * <:2, 3, 4:>" "20";
    interp_test "interp vector cross" "<:1, 2, 3:> :*: <:2, 3, 4:>"
      "< -1 2 -1 >";
    interp_test "interp vector mult" "<:1, 2, 3:> / 2. " "< 0.5 1. 1.5 >";
    interp_test "interp bop &&" "true && (false && true)" "false";
    interp_test "interp bop ||" "true || (false && true)" "true";
    interp_test "interp !" "!true" "false";
    interp_test "interp bop xor" "true || (false && true)" "true";
  ]

let suite =
  "test suite for Logic Parser"
  >::: List.flatten
         [
           eval_tests;
           parse_tests;
           vector_tests;
           set_tests;
           at_tests;
           to_string_tests;
           eq_tests;
           interp_tests;
         ]

let _ = run_test_tt_main suite
