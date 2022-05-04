(*
                         CS 51 Final Project
 *)

open CS51Utils ;;
open Absbook ;;
open Expr ;;

let free_vars_test () =
print_endline "hi" ;;

let new_varname_test () =
print_endline "bye" ;;

let subst_test () =
() ;;

let exp_to_concrete_string_test () =
() ;;

let exp_to_abstract_string_test () =
() ;;

let test_all () = 
  free_vars_test ();
  new_varname_test ();
  subst_test ();
  exp_to_concrete_string_test ();
  exp_to_abstract_string_test () ;;

let _ = test_all () ;;