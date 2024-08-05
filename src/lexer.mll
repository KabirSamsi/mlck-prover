{
  open Parser
  (** Lexer module used to lex strings into token streams. OCaml uses the 
   rules in [read] to generate a lexer. *)

  exception LexingError of string
  (** Execption raised for unrecognized strings *)
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = digit+
let neg_int = '(''-'int ')'
let float = digit+ '.' digit* | '.' digit+
let neg_float = '(''-'float ')'
let letter = ['a'-'z' 'A'-'Z']
let string = ['"'] [^'\n''"']* ['"']
let char = ['''] [^'\n''"'] [''']
let first_upper = ['A'-'Z']['a'-'z']*
let freevar = ['a'-'z']+
let entityvar = first_upper
let func = first_upper '='
let pred = first_upper ':'


rule read = 
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | ">" { GT }
  | "<" { LT }
  | ">=" { GEQ } 
  | "<=" { LEQ }
  | "==" { EQ }
  | "!=" { NEQ }
  | "*" { TIMES }
  | "/" { DIV }
  | "+" { PLUS }
  | "-" { MINUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "&&" { AND }
  | "||" { OR }
  | "XOR" { XOR }
  | "!" { NOT }
  | ":*:" { CROSS }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "{+}" { SETADD }
  | "{-}" { SETREMOVE }
  | "{U}" { UNION }
  | "{I}" { INTERSECTION }
  | "{S}" { SUBSET }
  | "{=}" { SETEQUALS }
  | "," { COMMA }
  | "." { PERIOD }
  | "inv" { INV }
  | "<:" { LVEC }
  | ":>" { RVEC }
  | "->" { IMPLIES }
  | "exists" { EXISTS }
  | "forall" { FORALL }
  | freevar { FREE (Lexing.lexeme lexbuf) }
  | entityvar { ENTITY (Lexing.lexeme lexbuf) }
  | func { FUNC (Lexing.lexeme lexbuf) }
  | pred { PRED (Lexing.lexeme lexbuf) }
  | string { STRING (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | neg_int as s { INT (int_of_string (String.sub s 1 (String.length s - 2)))}
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | neg_float as s { FLOAT (float_of_string (String.sub s 1 (String.length s - 2)))}
  | char { CHAR (String.get (Lexing.lexeme lexbuf) 1) }
  | eof { EOF }
  | _ as x { raise (LexingError ("unexpected character " ^ String.make 1 x)) }