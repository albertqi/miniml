(*
                         CS 51 Final Project
 *)

open CS51Utils ;;
open Absbook ;;
open Expr ;;
open Evaluation ;;

(*                       SAMPLE TESTING EXPRESSIONS                       *)

(* let a = ~-7 + 5 in
   let b = 1 * 2 * ~-5 in
   let c = a - b in c / 4 *)
let complex_nums =
  Let("a", Binop(Plus, Unop(Negate, Num(7)), Num(5)), Let("b", Binop(Times,
  Binop(Times, Num(1), Num(2)), Unop(Negate, Num(5))), Let("c", Binop(Minus,
  Var("a"), Var("b")), Binop(Divides, Var("c"), Num(4))))) ;;

(* let a = ~-.(3. ** 1. ** 4.) in
   let b = cos 0. +. sin 0. +. tan 0. in
   let c = log 1. in
   let d = (11. -. 11.5 *. 2.) in
   let e = if a = ~-.81. then d /. 4. else d /. 3. in
   b +. c +. e *)
let complex_floats =
  Let("a", Unop(NegateFloat, Binop(Power, Float(3.), Binop(Power, Float(1.),
  Float(4.)))), Let("b", Binop(PlusFloat, Binop(PlusFloat, Unop(Cosine,
  Float(0.)), Unop(Sine, Float(0.))), Unop(Tangent, Float(0.))), Let("c",
  Unop(NaturalLog, Float(1.)), Let("d", Binop(MinusFloat, Float(11.),
  Binop(TimesFloat, Float(11.5), Float(2.))), Let("e",
  Conditional(Binop(Equals, Var("a"), Unop(NegateFloat, Float(81.))),
  Binop(DividesFloat, Var("d"), Float(4.)), Binop(DividesFloat, Var("d"),
  Float(3.))), Binop(PlusFloat, Binop(PlusFloat, Var("b"),
  Var("c")), Var("e"))))))) ;;

(* if (fun b -> not b) (5 <= 5) then true else
     if (5 <> 7) = ("a" < "ab") then
       let f = fun x -> x > 7 in f 10 = (~-.3. >= ~-.1.)
     else true *)
let complex_bools =
  Conditional(App(Fun("b", Unop(Not, Var("b"))), Binop(LessThanEquals, Num(5),
  Num(5))), Bool(true), Conditional(Binop(Equals, Binop(NotEquals, Num(5),
  Num(7)), Binop(LessThan, String("a"), String("ab"))), Let("f", Fun("x",
  Binop(GreaterThan, Var("x"), Num(7))), Binop(Equals, App(Var("f"), Num(10)),
  Binop(GreaterThanEquals, Unop(NegateFloat, Float(3.)), Unop(NegateFloat,
  Float(1.))))), Bool(true))) ;;

(* let a = "a" ^ (fun s -> s ^ "c") "b" ^ "d" in
   a ^ (fun s -> s ^ s ^ s) "e" *)
let complex_strings =
  Let("a", Binop(Concatenate, String("a"), Binop(Concatenate, App(Fun("s",
  Binop(Concatenate, Var("s"), String("c"))), String("b")), String("d"))),
  Binop(Concatenate, Var("a"), App(Fun("s", Binop(Concatenate, Var("s"),
  Binop(Concatenate, Var("s"), Var("s")))), String("e")))) ;;

(* let rec f = fun x -> if x <= 1 then x else f (x - 1) + f (x - 2) in f 20 *)
let fib20 =
  Letrec("f", Fun("x", Conditional(Binop(LessThanEquals, Var("x"), Num(1)),
  Var("x"), Binop(Plus, App(Var("f"), Binop(Minus, Var("x"), Num(1))),
  App(Var("f"), Binop(Minus, Var("x"), Num(2)))))), App(Var("f"), Num(20))) ;;

(* let rec f = fun x -> if x <= 1 then 1 else x * f (x - 1) in f 20 *)
let fact20 =
  Letrec("f", Fun("x", Conditional(Binop(LessThanEquals, Var("x"), Num(1)),
  Num(1), Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"),
  Num(1)))))), App(Var("f"), Num(20))) ;;

(* let x = () in (fun () -> 7) x *)
let unit_function = Let("x", Unit, App(FunUnit(Unit, Num(7)), Var("x"))) ;;

(* let f = fun x -> fun y -> x + y in f 5 7 *)
let simple_curried =
  Let("f", Fun("x", Fun("y", Binop(Plus, Var("x"), Var("y")))),
  App(App(Var("f"), Num(5)), Num(7))) ;;

(* let x = 1 in
   let f = fun y -> fun z -> z * (x - y) in
   let y = 2 in f 3 4 *)
let complex_curried =
  Let("x", Num(1), Let("f", Fun("y", Fun("z", Binop(Times, Var("z"),
  Binop(Minus, Var("x"), Var("y"))))), Let("y", Num(2), App(App(Var("f"),
  Num(3)), Num(4))))) ;;

(* let x = 0 in let f = fun y -> y + x in let x = 7 in f 5 *)
let simple_dynamic_test =
  Let("x", Num(0), Let("f", Fun("y", Binop(Plus, Var("y"), Var("x"))),
  Let("x", Num(7), App(Var("f"), Num(5))))) ;;

(*                     SAMPLE TESTING ENVIRONMENTS                     *)

(* Empty environment *)
let empty = Env.empty () ;;

(* {x -> 7} *)
let simple_env = ref (Env.Val (Num 7)) |> Env.extend empty "x" ;;

(* {f -> [{x -> 7} ⊢ fun y -> x]; x -> 7} *)
let complex_env = ref (Env.Closure (Fun("y", Var "x"), simple_env)) |>
                  Env.extend simple_env "f" ;;

(* {x -> 5; f -> [{x -> 7} ⊢ fun y -> x]} *)
let override_env = ref (Env.Val (Num 5)) |> Env.extend complex_env "x" ;;

(*                       TESTING BEGINS                       *)

let close_test () =
  unit_test (Env.close (Num 7) empty = Env.Closure ((Num 7), empty))
            "close simple empty";
  unit_test (Env.close complex_floats empty =
             Env.Closure (complex_floats, empty))
            "close complex floats empty env";
  unit_test (Env.close complex_bools complex_env =
             Env.Closure (complex_bools, complex_env))
            "close complex bools complex env" ;;

let extend_env_to_string_test () =
  unit_test (Env.env_to_string empty = "{}")
            "extend_env_to_string_test empty";
  unit_test (Env.env_to_string simple_env = "{x -> 7}")
            "extend_env_to_string_test simple";
  unit_test (Env.env_to_string complex_env =
             "{f -> [{x -> 7} ⊢ fun y -> x]; x -> 7}")
            "extend_env_to_string_test complex";
  unit_test (Env.env_to_string override_env =
             "{x -> 5; f -> [{x -> 7} ⊢ fun y -> x]}")
            "extend_env_to_string_test override" ;;

let lookup_value_to_string_test () =
  unit_test (try Env.lookup empty "x" <> Env.lookup empty "x"
             with EvalError _ -> true | _ -> false)
            "lookup empty";
  unit_test (try Env.lookup simple_env "y" <> Env.lookup simple_env "y"
             with EvalError _ -> true | _ -> false)
            "lookup varid not found";
  unit_test (Env.lookup complex_env "f" =
             Env.Closure (Fun("y", Var "x"), simple_env))
            "lookup complex";
  unit_test (Env.lookup override_env "x" = Env.Val (Num 5))
            "lookup override";
  unit_test (Env.lookup complex_env "f" |> Env.value_to_string ~printenvp:true =
             "[{x -> 7} ⊢ fun y -> x]")
            "value_to_string complex";
  unit_test (Env.lookup override_env "x" |> Env.value_to_string ~printenvp:true =
             "5")
            "value_to_string override" ;;

(* Tests that apply to all of s, d, l, and e evaluators *)
let eval_all_test (eval : expr -> Env.env -> Env.value) =
  unit_test (eval (Binop (Plus, Num 1, Num 1)) (Env.empty ()) = Env.Val (Num 2))
            "eval all simple binop";
  unit_test (eval complex_nums empty = Env.Val (Num 2))
            "eval all complex nums";
  unit_test (eval complex_floats empty = Env.Val (Float ~-.3.))
            "eval all complex floats";
  unit_test (eval complex_bools empty = Env.Val (Bool false))
            "eval all complex bools";
  unit_test (eval complex_strings empty = Env.Val (String "abcdeee"))
            "eval all complex strings";
  unit_test (eval fib20 empty = Env.Val (Num 6765))
            "eval all fib20";
  unit_test (eval fact20 empty = Env.Val (Num 2432902008176640000))
            "eval all fact20";
  unit_test (eval unit_function empty = Env.Val (Num 7))
            "eval all unit function";
  unit_test (try eval (Unop(Not, Num 1)) empty <> eval (Unop(Not, Num 1)) empty
             with EvalError _ -> true | _ -> false)
            "eval all type mismatch not";
  unit_test (try eval (Unop(NegateFloat, Num 1)) empty <>
                 eval (Unop(NegateFloat, Num 1)) empty
             with EvalError _ -> true | _ -> false)
            "eval all type mismatch negate float";
  unit_test (try eval (Unop(Negate, Float 1.)) empty <>
                 eval (Unop(Negate, Float 1.)) empty
             with EvalError _ -> true | _ -> false)
            "eval all type mismatch negate";
  unit_test (try eval (Binop(PlusFloat, Num 1, Num 1)) empty <>
                 eval (Binop(PlusFloat, Num 1, Num 1)) empty
             with EvalError _ -> true | _ -> false)
            "eval all type mismatch plus float";
  unit_test (try eval (Binop(Minus, Float 1., Float 1.)) empty <>
                 eval (Binop(Minus, Float 1., Float 1.)) empty
             with EvalError _ -> true | _ -> false)
            "eval all type mismatch minus";
  unit_test (try eval (Binop(Equals, Bool true, Num 1)) empty <>
                 eval (Binop(Equals, Bool true, Num 1)) empty
             with EvalError _ -> true | _ -> false)
            "eval all type mismatch equals" ;;

let eval_s_test () =
  eval_all_test eval_s;
  unit_test (eval_s simple_curried empty = Env.Val (Num 12))
            "eval_s simple curried";
  unit_test (eval_s complex_curried empty = Env.Val (Num ~-8))
            "eval_s complex curried";
  unit_test (eval_s simple_dynamic_test empty = Env.Val (Num 5))
            "eval_s simple dynamic" ;;

let eval_d_test () =
  eval_all_test eval_d;
  unit_test (try eval_d simple_curried empty <> eval_d simple_curried empty
             with EvalError _ -> true | _ -> false)
            "eval_d simple curried";
  unit_test (eval_d complex_curried empty = Env.Val (Num ~-4))
            "eval_d complex curried";
  unit_test (eval_d simple_dynamic_test empty = Env.Val (Num 12))
            "eval_d simple dynamic" ;;

let eval_l_test () =
  eval_all_test eval_l;
  unit_test (eval_l simple_curried empty = Env.Val (Num 12))
            "eval_l simple curried";
  unit_test (eval_l complex_curried empty = Env.Val (Num ~-8))
            "eval_l complex curried";
  unit_test (eval_l simple_dynamic_test empty = Env.Val (Num 5))
            "eval_l simple dynamic" ;;

(* Additionally, test lazy expressions evaluated with eval_e *)
let eval_e_test () =
  eval_all_test eval_e;
  unit_test (eval_e simple_curried empty = Env.Val (Num 12))
            "eval_e simple curried";
  unit_test (eval_e complex_curried empty = Env.Val (Num ~-8))
            "eval_e complex curried";
  unit_test (eval_e simple_dynamic_test empty = Env.Val (Num 5))
            "eval_e simple dynamic";
  (* force lazy 7 *)
  let simple_lazy = Unop(Force, Lazy(ref (Num 7))) in
  unit_test (eval_e simple_lazy empty = Env.Val (Num 7))
            "eval_e simple lazy";
  (* force (force (lazy (lazy 7))) *)
  let stacked_lazy = Unop(Force, Unop(Force, Lazy(ref (Lazy(ref (Num 7)))))) in
  unit_test (eval_e stacked_lazy empty = Env.Val (Num 7))
            "eval_e stacked lazy";
  (* let rec forever = fun x -> 0 + forever x in
     let f = forever 0 in if true then 7 else f *)
  let simple_forever =
    Letrec("forever", Fun("x", Binop(Plus, Num(0), App(Var("forever"),
    Var("x")))), Let("f", App(Var("forever"), Num(0)), Conditional(Bool(true),
    Num(7), Var("f")))) in
  unit_test (try eval_e simple_forever empty <> eval_e simple_forever empty
             with Stack_overflow -> true | _ -> false)
            "eval_e simple forever";
  (* let rec forever = fun x -> 0 + forever x in
     let f = fun () -> forever 0 in if true then 7 else f () *)
  let delay_forever =
    Letrec("forever", Fun("x", Binop(Plus, Num(0), App(Var("forever"),
    Var("x")))), Let("f", FunUnit(Unit, App(Var("forever"), Num(0))),
    Conditional(Bool(true), Num(7), App(Var("f"), Unit)))) in
  unit_test (eval_e delay_forever empty = Env.Val (Num 7))
            "eval_e delay forever";
  (* let rec forever = fun x -> 0 + forever x in
     let f = lazy (forever 0) in if true then 7 else force f *)
  let lazy_forever =
    Letrec("forever", Fun("x", Binop(Plus, Num(0), App(Var("forever"),
    Var("x")))), Let("f", Lazy(ref (App(Var("forever"), Num(0)))),
    Conditional(Bool(true), Num(7), Unop(Force, Var("f"))))) in
  unit_test (eval_e lazy_forever empty = Env.Val (Num 7))
            "eval_e lazy forever";
  (* let y = 7 in (force (lazy (fun x -> y))) 0 *)
  let lazy_function =
    Let("y", Num(7), App(Unop(Force, Lazy(ref (Fun("x", Var("y"))))), Num 0)) in
  unit_test (eval_e lazy_function empty = Env.Val (Num 7))
            "eval_e lazy function";
  (* let helper = fun x -> x in let lazy_helper = fun x -> lazy (helper x) in
     let res = lazy_helper 7 in force res *)
  let lazy_more =
    Let("helper", Fun("x", Var("x")), Let("lazy_helper", Fun("x",
    Lazy(ref (App(Var("helper"), Var("x"))))), Let("res",
    App(Var("lazy_helper"), Num(7)), Unop(Force, Var("res"))))) in
  unit_test (eval_e lazy_more empty = Env.Val (Num 7))
            "eval_e lazy more" ;;

let test_all () = 
  close_test ();
  extend_env_to_string_test ();
  lookup_value_to_string_test ();
  eval_s_test ();
  eval_d_test ();
  eval_l_test ();
  eval_e_test () ;;

let _ = test_all () ;;