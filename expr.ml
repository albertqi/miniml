(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | NegateFloat
  | Not
  | NaturalLog
  | Sine
  | Cosine
  | Tangent
  | PrintString
  | PrintEndline
  | Force
;;
    
type binop =
  | Plus
  | PlusFloat
  | Minus
  | MinusFloat
  | Times
  | TimesFloat
  | Divides
  | DividesFloat
  | Power
  | Equals
  | NotEquals
  | LessThan
  | GreaterThan
  | LessThanEquals
  | GreaterThanEquals
  | Concatenate
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Bool of bool                         (* booleans *)
  | String of string                     (* strings *)
  | Lazy of expr ref                     (* lazy expressions *)
  | Unit                                 (* units *)
  | Sequence of expr * expr              (* sequences *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | FunUnit of expr * expr               (* unit function definitions *)
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
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Num _ | Float _ | Bool _ | String _ | Unit | Raise | Unassigned -> SS.empty
  | Var v -> SS.singleton v
  | Lazy _ -> SS.empty
  | Unop (_, e)
  | FunUnit (_, e) -> free_vars e
  | Binop (_, e1, e2)
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) ->
    SS.union (free_vars e1) (free_vars e2) |> SS.union (free_vars e3)
  | Fun (v, e) -> free_vars e |> SS.remove v
  | Let (v, e1, e2) ->
    free_vars e2 |> SS.remove v |> SS.union (free_vars e1)
  | Letrec (v, e1, e2) ->
    free_vars e2 |> SS.remove v |> SS.union (free_vars e1 |> SS.remove v) ;;

let gensym : string -> string =
  let count = ref 0 in
  fun s -> let res = s ^ string_of_int !count
           in count := !count + 1;
           res ;;

(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname () : varid =
  gensym "var" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  if free_vars exp |> SS.mem var_name |> not then exp
  else
  match exp with
  | Num _ | Float _ | Bool _ | String _ | Unit | Raise | Unassigned -> exp
  | Var v -> if v = var_name then repl else Var v
  | Lazy _ -> exp
  | Unop (u, e) -> Unop (u, subst var_name repl e)
  | Binop (b, e1, e2) -> Binop (b, subst var_name repl e1, subst var_name repl e2)
  | Conditional (e1, e2, e3) ->
    let e1_subst, e2_subst, e3_subst = subst var_name repl e1,
                                       subst var_name repl e2,
                                       subst var_name repl e3
    in Conditional (e1_subst, e2_subst, e3_subst)
  | Fun (v, e) ->
    if v = var_name then exp
    else if free_vars repl |> SS.mem v |> not then Fun (v, subst var_name repl e)
    else let new_v = new_varname ()
         in Fun (new_v, subst v (Var new_v) e |> subst var_name repl)
  | FunUnit (e1, e2) -> FunUnit (e1, subst var_name repl e2)
  | Let (v, e1, e2)
  | Letrec (v, e1, e2) ->
    if v = var_name then Let (v, subst var_name repl e1, e2)
    else if free_vars repl |> SS.mem v |> not
         then Let (v, subst var_name repl e1, subst var_name repl e2)
         else let new_v = new_varname ()
              in Let (new_v,
                      subst var_name repl e1,
                      subst v (Var new_v) e2 |> subst var_name repl)
  | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2) ;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v
  | Num n -> string_of_int n
  | Float n -> string_of_float n
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Lazy e -> "lazy (" ^ exp_to_concrete_string !e ^ ")"
  | Unit -> "()"
  | Sequence (e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in e1_str ^ "; " ^ e2_str
  | Unop (u, e) ->
    let e_str = exp_to_concrete_string e
    in let unop_str =
      match u with
      | Negate -> "~-"
      | NegateFloat -> "~-."
      | Not -> "not "
      | NaturalLog -> "log "
      | Sine -> "sin "
      | Cosine -> "cos "
      | Tangent -> "tan "
      | PrintString -> "print_string "
      | PrintEndline -> "print_endline "
      | Force -> "force "
    in unop_str ^ e_str
  | Binop (b, e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, exp_to_concrete_string e2
    in let binop_str =
      match b with
      | Plus -> " + "
      | PlusFloat -> " +. "
      | Minus -> " - "
      | MinusFloat -> " -. "
      | Times -> " * "
      | TimesFloat -> " *. "
      | Divides -> " / "
      | DividesFloat -> " /. "
      | Power -> " ** "
      | Equals -> " = "
      | NotEquals -> " <> "
      | LessThan -> " < "
      | GreaterThan -> " > "
      | LessThanEquals -> " <= "
      | GreaterThanEquals -> " >= "
      | Concatenate -> " ^ "
    in e1_str ^ binop_str ^ e2_str
  | Conditional (e1, e2, e3) ->
    let e1_str, e2_str, e3_str = exp_to_concrete_string e1,
                                 exp_to_concrete_string e2,
                                 exp_to_concrete_string e3
    in "if " ^ e1_str ^ " then " ^ e2_str ^ " else " ^ e3_str
  | Fun (v, e) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string e
  | FunUnit (_, e) -> "fun () -> " ^ exp_to_concrete_string e
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
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var(\"" ^ v ^ "\")"
  | Num n -> "Num(" ^ string_of_int n ^ ")"
  | Float n -> "Float(" ^ string_of_float n ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | String s -> "String(\"" ^ s ^ "\")"
  | Lazy e -> "Lazy(ref " ^ exp_to_abstract_string !e ^ ")"
  | Unit -> "Unit"
  | Sequence (e1, e2) -> 
    let e1_str, e2_str = exp_to_abstract_string e1, exp_to_abstract_string e2
    in "Sequence(" ^ e1_str ^ ", " ^ e2_str ^ ")"
  | Unop (u, e) ->
    let e_str = exp_to_abstract_string e
    in let unop_str =
      match u with
      | Negate -> "Negate"
      | NegateFloat -> "NegateFloat"
      | Not -> "Not"
      | NaturalLog -> "NaturalLog"
      | Sine -> "Sine"
      | Cosine -> "Cosine"
      | Tangent -> "Tangent"
      | PrintString -> "PrintString"
      | PrintEndline -> "PrintEndline"
      | Force -> "Force"
    in "Unop(" ^ unop_str ^ ", " ^ e_str ^ ")"
  | Binop (b, e1, e2) ->
    let e1_str, e2_str = exp_to_abstract_string e1, exp_to_abstract_string e2
    in let binop_str =
      match b with
      | Plus -> "Plus"
      | PlusFloat -> "PlusFloat"
      | Minus -> "Minus"
      | MinusFloat -> "MinusFloat"
      | Times -> "Times"
      | TimesFloat -> "TimesFloat"
      | Divides -> "Divides"
      | DividesFloat -> "DividesFloat"
      | Power -> "Power"
      | Equals -> "Equals"
      | NotEquals -> "NotEquals"
      | LessThan -> "LessThan"
      | GreaterThan -> "GreaterThan"
      | LessThanEquals -> "LessThanEquals"
      | GreaterThanEquals -> "GreaterThanEquals"
      | Concatenate -> "Concatenate"
    in "Binop(" ^ binop_str ^ ", " ^ e1_str ^ ", " ^ e2_str ^ ")"
  | Conditional (e1, e2, e3) ->
    let e1_str, e2_str, e3_str = exp_to_abstract_string e1,
                                 exp_to_abstract_string e2,
                                 exp_to_abstract_string e3
    in "Conditional(" ^ e1_str ^ ", " ^ e2_str ^ ", " ^ e3_str ^ ")"
  | Fun (v, e) -> "Fun(\"" ^ v ^ "\", " ^ exp_to_abstract_string e ^ ")"
  | FunUnit (_, e) -> "FunUnit(Unit, " ^ exp_to_abstract_string e ^ ")"
  | Let (v, e1, e2) ->
    let e1_str, e2_str = exp_to_abstract_string e1, exp_to_abstract_string e2
    in "Let(\"" ^ v ^ "\", " ^ e1_str ^ ", " ^ e2_str ^ ")"
  | Letrec (v, e1, e2) ->
    let e1_str, e2_str = exp_to_abstract_string e1, exp_to_abstract_string e2
    in "Letrec(\"" ^ v ^ "\", " ^ e1_str ^ ", " ^ e2_str ^ ")"
  | Raise -> "parse error"
  | Unassigned -> "unassigned"
  | App (e1, e2) ->
    let e1_str, e2_str = exp_to_abstract_string e1, exp_to_abstract_string e2
    in "App(" ^ e1_str ^ ", " ^ e2_str ^ ")"
