(* test_cases.ml *)

#use "pc.ml";;
#use "hw1.ml";;
open PC;;
open InfixParser;;


(* Function to convert expr to string *)
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

(* Function to convert parsing_result to string *)
let string_of_parsing_result { index_from; index_to; found } =
  Printf.sprintf "{ index_from = %d; index_to = %d; found = %s }"
    index_from index_to (string_of_expr found)
;;

(* Function to run a test and print test and output *)
let run_test n input =
  Printf.printf "Test %d: test_string nt_expr \"%s\" 0;;\n" n input;
  let result = test_string nt_expr input 0 in
  Printf.printf "Output: - : expr parsing_result = %s\n\n" (string_of_parsing_result result)
;;

(* Run your test cases *)

(*part 2 test cases:*)
run_test 1 "1";;
run_test 2 "x";;
run_test 3 "x + y";;
run_test 4 "x + y * z ^ t";;
run_test 5 "2 * 3 + 4 * 5";;
run_test 6 "1 + 2 * 3 + 4";;
run_test 7 "(1 + 2) * (3 + 4)";;
run_test 8 "1 * 2 * 3 * 4 * 5";;
run_test 9 "1 ^ 2 ^ 3 ^ 4 ^ 5";;
run_test 10 "a(b, c)";;
run_test 11 "f(x, 1, y, 2)";;
run_test 12 "f()";;
run_test 13 "f() ()";;
run_test 14 "f()(x)()(y)";;
run_test 15 "A[i]";;
run_test 16 "A[3]";;
run_test 17 "A[3][4]";;
run_test 18 "f(1, 2)[3](4)[5](6, 7, 8)";;
run_test 19 "a[1] + a[2] * a[3] ^ a[4]";;
run_test 20 "1 + (2) * ((3)) ^ (((4)))";;
run_test 21 "1 + -1";;
run_test 22 "1 + -1 - -3";;
run_test 23 "1 + -1 - -3 + (- x)";;
run_test 24 "(/ x) * (- y)";;

(*part 2.1 test cases*)
run_test 25 "2 * 3 + 4%";;
run_test 26 "2 / 3 + 4%";;
run_test 27 "2 / 3 + (2 + 5%)%";;
run_test 28 "2 + 50%";;
run_test 29 "5 * 2 + 50%";;
run_test 30 "5 * 2 + (50)%";;
run_test 31 "5 * 2 + (50 - 25%)%";;
run_test 32 "5 * 5%";;
run_test 33 "6 + 5 * 5%";;
run_test 34 "7 / 6 + 5 * 5%";;
run_test 35 "7 / (6 + 5 * 5%)";;
run_test 36 "10 / 5 - 20%";;
run_test 37 "10 / 5 - 20% + 100%";;
run_test 38 "10 / 5 - 20% + 100% - 6%";;