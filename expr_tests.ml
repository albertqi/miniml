(*
                         CS 51 Final Project
 *)

open CS51Utils ;;
open Absbook ;;
open Expr ;;

(*                       SAMPLE TESTING EXPRESSIONS                       *)

(* (fun x -> x) (fun y -> y * 2) 1 *)
let empty =
  App(App(Fun("x", Var("x")), Fun("y", Binop(Times, Var("y"),
  Num(2)))), Num(1)) ;;

(* let f = fun x -> x + y in f x *)
let simple =
  Let("f", Fun("x", Binop(Plus, Var("x"), Var("y"))),
  App(Var("f"), Var("x"))) ;;

(* let f = fun x -> fun y -> if x > y then y else f x (y - 1) in f 7 5 *)
let complex =
  Let("f", Fun("x", Fun("y", Conditional(Binop(GreaterThan, Var("x"),
  Var("y")), Var("y"), App(App(Var("f"), Var("x")), Binop(Minus, Var("y"),
  Num(1)))))), App(App(Var("f"), Num(7)), Num(5))) ;;

(*                       TESTING BEGINS                       *)

let free_vars_test () =
  unit_test (free_vars empty |> same_vars (vars_of_list []))
            "free_vars empty";
  unit_test (free_vars simple |> same_vars (vars_of_list ["x"; "y"]))
            "free_vars simple";
  unit_test (free_vars complex |> same_vars (vars_of_list ["f"]))
            "free_vars complex" ;;

let new_varname_test () =
  unit_test (new_varname () = "var0")
            "new_varname 0";
  unit_test (new_varname () = "var1")
            "new_varname 1";
  unit_test (new_varname () |> ignore; new_varname () = "var3")
            "new_varname 3" ;;

let subst_test () =
  unit_test (subst "x" (Num 7) empty = empty)
            "subst empty";
  unit_test (subst "x" (Num 7) simple =
             Let ("f", Fun ("x", Binop (Plus, Var "x", Var "y")),
             App (Var "f", Num 7)))
            "subst simple x";
  unit_test (subst "x" (Num 7) simple |> subst "y" (Num 5) =
             Let ("f", Fun ("x", Binop (Plus, Var "x", Num 5)),
             App (Var "f", Num 7)))
            "subst simple x y";
  unit_test (subst "x" (Num 7) complex = complex)
            "subst complex no change";
  unit_test (subst "f" (Fun("x", Fun("y", Var("x")))) complex =
             Let ("f", Fun ("x", Fun ("y", Conditional (Binop (GreaterThan,
             Var "x", Var "y"), Var "y", App (App (Fun ("x", Fun ("y",
             Var "x")), Var "x"), Binop (Minus, Var "y", Num 1))))),
             App (App(Var "f", Num 7), Num 5)))
            "subst complex f" ;;

let exp_to_concrete_string_test () =
  unit_test (exp_to_concrete_string empty =
             "((fun x -> x) (fun y -> y * 2)) (1)")
            "exp_to_concrete_string empty";
  unit_test (exp_to_concrete_string simple =
             "let f = fun x -> x + y in (f) (x)")
            "exp_to_concrete_string simple";
  unit_test (exp_to_concrete_string complex =
             "let f = fun x -> fun y -> \
              if x > y then y else ((f) (x)) (y - 1) in ((f) (7)) (5)")
            "exp_to_concrete_string complex" ;;

let exp_to_abstract_string_test () =
  unit_test (exp_to_abstract_string empty =
             "App(App(Fun(\"x\", Var(\"x\")), Fun(\"y\", Binop(Times, \
              Var(\"y\"), Num(2)))), Num(1))")
            "exp_to_abstract_string empty";
  unit_test (exp_to_abstract_string simple =
             "Let(\"f\", Fun(\"x\", Binop(Plus, Var(\"x\"), Var(\"y\"))), \
              App(Var(\"f\"), Var(\"x\")))")
            "exp_to_abstract_string simple";
  unit_test (exp_to_abstract_string complex =
             "Let(\"f\", Fun(\"x\", Fun(\"y\", Conditional(Binop(GreaterThan, \
              Var(\"x\"), Var(\"y\")), Var(\"y\"), App(App(Var(\"f\"), \
              Var(\"x\")), Binop(Minus, Var(\"y\"), Num(1)))))), \
              App(App(Var(\"f\"), Num(7)), Num(5)))")
            "exp_to_abstract_string complex" ;;

let test_all () = 
  free_vars_test ();
  new_varname_test ();
  subst_test ();
  exp_to_concrete_string_test ();
  exp_to_abstract_string_test () ;;

let _ = test_all () ;;