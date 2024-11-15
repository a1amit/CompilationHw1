(* test_hw1.ml *)

#use "pc.ml";;
#use "hw1.ml";;

open PC;;
open InfixParser;;

(* Helper function to convert expr to string *)
let rec string_of_expr e =
  match e with
  | Num n -> Printf.sprintf "Num %d" n
  | Var v -> Printf.sprintf "Var \"%s\"" v
  | BinOp (op, e1, e2) ->
    let op_str = match op with
      | Add -> "Add"
      | Sub -> "Sub"
      | Mul -> "Mul"
      | Div -> "Div"
      | Mod -> "Mod"
      | Pow -> "Pow"
      | AddPer -> "AddPer"
      | SubPer -> "SubPer"
      | PerOf -> "PerOf"
    in
    Printf.sprintf "BinOp (%s, %s, %s)" op_str (string_of_expr e1) (string_of_expr e2)
  | Deref (e1, e2) ->
    Printf.sprintf "Deref (%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Call (e, args) ->
    let args_str = String.concat "; " (List.map string_of_expr args) in
    Printf.sprintf "Call (%s, [%s])" (string_of_expr e) args_str
;;

let test_inputs = [
  "1";
  "x";
  "x + y";
  "x + y * z ^ t";
  "2 * 3 + 4 * 5";
  "1 + 2 * 3 + 4";
  "(1 + 2) * (3 + 4)";
  "1 * 2 * 3 * 4 * 5";
  "1 ^ 2 ^ 3 ^ 4 ^ 5";
  "a(b, c)";
  "f(x, 1, y, 2)";
  "f()";
  "f() ()";
  "f()(x)()(y)";
  "A[i]";
  "A [3]";
  "A [3][4]";
  "f(1 , 2)[5](6 , 7, 8)";
  "a[1] + a[2] * a[3] ^ a[4]";
  "1 + (2) * ((3)) ^ (((4)))";
  "1 + -1";
  "1 + -1 + -3";
  "x - -3";
  "x + -3 * -1 / x - y";
  "-1 + -2";
];;

let run_tests () =
  List.iteri (fun index input ->
    try
      let result = test_string nt_expr input 0 in
      Printf.printf "Test Case %d: Passed\n" (index + 1);
      Printf.printf "Input: %s\n" input;
      Printf.printf "Parsed Expression: %s\n\n" (string_of_expr result.found);
    with
    | PC.X_no_match ->
      Printf.printf "Test Case %d: Failed (No match)\nInput: %s\n\n" (index + 1) input
    | e ->
      Printf.printf "Test Case %d: Failed (%s)\nInput: %s\n\n" (index + 1) (Printexc.to_string e) input
  ) test_inputs;;

run_tests ();;
