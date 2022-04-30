(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let free_vars (exp : expr) : varidset =
  failwith "free_vars not implemented" ;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unop (u, e) ->
    (match u with | Negate -> "-" ^ exp_to_concrete_string e)
  | Binop (b, e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in let binop_str =
      match b with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Equals -> "="
      | LessThan -> "<"
    in e1_str ^ binop_str ^ e2_str
  | Conditional (e1, e2, e3) ->
    let e1_str, e2_str, e3_str = exp_to_concrete_string e1,
                                 exp_to_concrete_string e2,
                                 exp_to_concrete_string e3
    in "if " ^ e1_str ^ " then " ^ e2_str ^ " else " ^ e3_str
  | Fun (v, e) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string e
  | Let (v, e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in "let " ^ v ^ " = " ^ e1_str ^ " in " ^ e2_str
  | Letrec (v, e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in "let rec " ^ v ^ " = " ^ e1_str ^ " in " ^ e2_str
  | Raise -> "parse error"
  | Unassigned -> "unassigned"
  | App (e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in "(" ^ e1_str ^ ") (" ^ e2_str ^ ")"
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var(" ^ v ^ ")"
  | Num n -> "Num(" ^ string_of_int n ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop (u, e) ->
    (match u with | Negate -> "Unop(Negate, " ^ exp_to_concrete_string e ^ ")")
  | Binop (b, e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in let binop_str =
      match b with
      | Plus -> "Plus"
      | Minus -> "Minus"
      | Times -> "Times"
      | Equals -> "Equals"
      | LessThan -> "LessThan"
    in "Binop(" ^ binop_str ^ ", " ^ e1_str ^ ", " ^ "e2_str" ^ ")"
  | Conditional (e1, e2, e3) ->
    let e1_str, e2_str, e3_str = exp_to_concrete_string e1,
                                 exp_to_concrete_string e2,
                                 exp_to_concrete_string e3
    in "Conditional(" ^ e1_str ^ ", " ^ e2_str ^ ", " ^ e3_str ^ ")"
  | Fun (v, e) -> "Fun(" ^ v ^ ", " ^ exp_to_concrete_string e ^ ")"
  | Let (v, e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in "Let(" ^ v ^ ", " ^ e1_str ^ ", " ^ e2_str ^ ")"
  | Letrec (v, e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in "Letrec(" ^ v ^ ", " ^ e1_str ^ ", " ^ e2_str ^ ")"
  | Raise -> "parse error"
  | Unassigned -> "unassigned"
  | App (e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in "App(" ^ e1_str ^ ", " ^ e2_str ^ ")"
