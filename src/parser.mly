%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <char> CHAR
%token TRUE
%token FALSE
%token GT
%token LT
%token GEQ
%token LEQ
%token EQ
%token NEQ
%token TIMES
%token DIV
%token PLUS
%token MINUS
%token SETADD
%token SETREMOVE
%token UNION
%token INTERSECTION
%token SUBSET
%token SETEQUALS
%token LPAREN
%token RPAREN
%token AND
%token OR
%token XOR
%token CROSS
%token NOT
%token LBRACE
%token RBRACE
%token LVEC
%token RVEC
%token COMMA
%token INV
%token IMPLIES
%token EXISTS
%token FORALL
%token <string> FREE
%token <string> ENTITY
%token <string> FUNC
%token <string> PRED
%token PERIOD
%token EOF

%left OR
%left AND
%left XOR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV
%right NOT

%start <Ast.expr> prog
%%

prog:
  | e = expr; EOF { e }
  ;

var:
  | f = FREE { Free (String.sub f 1 (String.length f - 2)) }
  | e = ENTITY { Entity (String.sub e 1 (String.length e - 2)) }
  | func = FUNC; LPAREN; vlst = separated_list(COMMA, var); RPAREN { Function ((String.sub func 1 (String.length func - 2)), vlst) }

expr:
  | i = INT { Integer i }
  | f = FLOAT { Float f }
  | s = STRING { Str (String.sub s 1 (String.length s - 2)) }
  | c = CHAR { Character c }
  | TRUE { Boolean true }
  | FALSE { Boolean false }
  | e1 = expr; op = bop; e2 = expr { BopExpr (op, e1, e2) }
  | op = unop; e = expr { UnExpr (op, e) }
  | p = PRED; LPAREN; vlst = separated_list(COMMA, var); RPAREN { PredExpr ((String.sub p 1 (String.length p - 2)), vlst) }
  | q = quant; v = var; PERIOD; e = expr { QuantExpr (q, v, e) }
  | LPAREN; e=expr; RPAREN {e}
  | LBRACE; lst=separated_list(COMMA, expr); RBRACE { Set lst }
  | LVEC; lst=separated_list(COMMA, expr); RVEC { Vector lst }
  ;

%inline bop:
  | GT { Ast.Gt }
  | LT { Ast.Lt }
  | GEQ { Ast.Geq }
  | LEQ { Ast.Leq }
  | EQ { Ast.Eq }
  | NEQ { Ast.Neq }
  | TIMES { Ast.Mult }
  | DIV { Ast.Div }
  | PLUS { Ast.Add }
  | MINUS { Ast.Sub }
  | AND { Ast.And }
  | OR { Ast.Or }
  | XOR { Ast.Xor }
  | CROSS { Ast.Cross }
  | SETADD { Ast.Setadd }
  | SETREMOVE { Ast.Setremove }
  | UNION { Ast.Union }
  | INTERSECTION { Ast.Intersection }
  | SUBSET { Ast.Subset }
  | SETEQUALS { Ast.Setequals }
  | IMPLIES { Ast.Implies }

%inline unop:
  | NOT { Ast.Not }
  | INV { Ast.Inv }

%inline quant:
  | EXISTS { Ast.Exists }
  | FORALL { Ast.All }
