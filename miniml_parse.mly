(*
                         CS 51 Final Project
                           MiniML -- Parser
*)
                  
%{
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG NEGFLOAT
%token NOT
%token NATURALLOG
%token SINE COSINE TANGENT
%token PRINTSTRING PRINTENDLINE
%token PLUS PLUSFLOAT MINUS MINUSFLOAT
%token TIMES TIMESFLOAT DIVIDES DIVIDESFLOAT
%token POWER
%token EQUALS NOTEQUALS
%token LESSTHAN GREATERTHAN LESSTHANEQUALS GREATERTHANEQUALS
%token CONCATENATE
%token IF THEN ELSE
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token <float> FLOAT
%token <string> STRING
%token LAZY FORCE
%token UNIT
%token SEQUENCE
%token TRUE FALSE

%nonassoc IF
%left EQUALS NOTEQUALS
%left LESSTHAN GREATERTHAN LESSTHANEQUALS GREATERTHANEQUALS
%left PLUS PLUSFLOAT MINUS MINUSFLOAT
%left TIMES TIMESFLOAT DIVIDES DIVIDESFLOAT
%right POWER
%right CONCATENATE
%right SEQUENCE
%nonassoc NEG NEGFLOAT
%nonassoc NOT
%nonassoc NATURALLOG
%nonassoc SINE COSINE TANGENT
%nonassoc PRINTSTRING PRINTENDLINE
%nonassoc LAZY FORCE

%start input
%type <Expr.expr> input

(* Grammar follows *)
%%
input:  exp EOF                 { $1 }

exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

expnoapp: INT                   { Num $1 }
        | FLOAT                 { Float $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | STRING                { String $1 }
        | LAZY exp              { Lazy (ref $2) }
        | UNIT                  { Unit }
        | exp SEQUENCE exp      { Sequence($1, $3) }
        | ID                    { Var $1 }
        | NEG exp               { Unop(Negate, $2) }
        | NEGFLOAT exp          { Unop(NegateFloat, $2) }
        | NOT exp               { Unop(Not, $2) }
        | NATURALLOG exp        { Unop(NaturalLog, $2) }
        | SINE exp              { Unop(Sine, $2) }
        | COSINE exp            { Unop(Cosine, $2) }
        | TANGENT exp           { Unop(Tangent, $2) }
        | PRINTSTRING exp       { Unop(PrintString, $2) }
        | PRINTENDLINE exp      { Unop(PrintEndline, $2) }
        | FORCE exp             { Unop(Force, $2) }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp PLUSFLOAT exp     { Binop(PlusFloat, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp MINUSFLOAT exp    { Binop(MinusFloat, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp TIMESFLOAT exp    { Binop(TimesFloat, $1, $3) }
        | exp DIVIDES exp       { Binop(Divides, $1, $3) }
        | exp DIVIDESFLOAT exp  { Binop(DividesFloat, $1, $3) }
        | exp POWER exp         { Binop(Power, $1, $3) }
        | exp EQUALS exp                { Binop(Equals, $1, $3) }
        | exp NOTEQUALS exp             { Binop(NotEquals, $1, $3) }
        | exp LESSTHAN exp              { Binop(LessThan, $1, $3) }
        | exp GREATERTHAN exp           { Binop(GreaterThan, $1, $3) }
        | exp LESSTHANEQUALS exp        { Binop(LessThanEquals, $1, $3) }
        | exp GREATERTHANEQUALS exp     { Binop(GreaterThanEquals, $1, $3) }
        | exp CONCATENATE exp           { Binop(Concatenate, $1, $3) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp           { Fun($2, $4) }
        | FUNCTION UNIT DOT exp         { FunUnit(Unit, $4) }
        | RAISE                         { Raise }
        | OPEN exp CLOSE                { $2 }
;

%%
