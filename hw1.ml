(* hw1.ml
 * Handling infix expressions with percents:
 *
 *   x + y %
 *   x - y %
 *   x * y %
 *
 * Programmer: Mayer Goldberg, 2024
 *)

#use "pc.ml";;

type binop = Add | Sub | Mul | Div | Mod | Pow | AddPer | SubPer | PerOf;;

type expr =
  | Num of int
  | Var of string
  | BinOp of binop * expr * expr
  | PerExpr of expr        (* Expression followed by '%' *)
  | Deref of expr * expr
  | Call of expr * expr list;;

module type INFIX_PARSER = sig
  val nt_expr : expr PC.parser
end;;

module InfixParser : INFIX_PARSER = struct
  open PC;;

  (* Helper parsers for whitespace *)
  let nt_whitespace = const (fun ch -> ch <= ' ');;
  let nt_skip_whitespace = star nt_whitespace;;

  let make_char_ws ch =
    pack (caten nt_skip_whitespace (caten (char ch) nt_skip_whitespace))
         (fun (_, (c, _)) -> c);;

  let make_word_ws s =
    pack (caten nt_skip_whitespace (caten (word s) nt_skip_whitespace))
         (fun (_, (w, _)) -> w);;

  (* Parser for numbers *)
  let nt_digit = range '0' '9';;
  let nt_nat =
    pack (plus nt_digit)
         (fun ds -> int_of_string (string_of_list ds));;

  let nt_integer =
    pack (caten (maybe (char '-')) nt_nat)
         (fun (sign, n) ->
            match sign with
            | Some _ -> -n
            | None -> n);;

  let nt_number s i =
    let nt =
      pack
        (caten nt_skip_whitespace
          (caten nt_integer (maybe (make_char_ws '%'))))
        (fun (_, (n, perc_opt)) ->
           match perc_opt with
           | Some _ -> PerExpr (Num n)
           | None -> Num n)
    in nt s i

  (* Parser for variables *)
  let nt_var_start = disj (range 'a' 'z') (range 'A' 'Z');;
  let nt_var_char =
    disj_list [nt_var_start; range '0' '9'; char '$'; char '_'];;

  let nt_var_name = plus nt_var_char;;

  let nt_variable =
    pack (caten nt_skip_whitespace nt_var_name)
         (fun (_, cs) ->
            let var_name = string_of_list cs in
            if var_name = "mod" then raise X_no_match else Var var_name);;

  (* Parser for parenthesized expressions *)
  let rec nt_paren_expr s i =
    let nt =
      pack (caten (make_char_ws '(') (caten nt_expr (make_char_ws ')')))
           (fun (_, (e, _)) -> e)
    in nt s i

  (* Parser for atomic expressions *)
  and nt_atomic s i =
    (disj_list [nt_number; nt_variable; nt_paren_expr]) s i

  (* Parser for function arguments *)
  and nt_args s i =
    let nt =
      pack (caten (make_char_ws '(')
                  (caten (maybe (make_separated_by_star (make_char_ws ',') nt_expr)) (make_char_ws ')')))
           (fun (_, (args_opt, _)) ->
              match args_opt with
              | Some args -> args
              | None -> [])
    in nt s i

  (* Parser for function calls *)
  and nt_call_op s i =
    let nt = pack nt_args (fun args -> fun e -> Call (e, args)) in
    nt s i

  (* Parser for array indexing *)
  and nt_indexing s i =
    let nt =
      pack (caten (make_char_ws '[') (caten nt_expr (make_char_ws ']')))
           (fun (_, (e, _)) -> e)
    in nt s i

  and nt_index_op s i =
    let nt = pack nt_indexing (fun idx -> fun e -> Deref (e, idx)) in
    nt s i

  (* Parser for postfix operators *)
  and nt_postfix_op s i =
    (disj nt_call_op nt_index_op) s i

  (* Parser for expressions with postfix operators *)
  and nt_postfix s i =
    let res_atomic = nt_atomic s i in
    let rec parse_postfix e index_from index_to =
      try
        (* Handle postfix '%' operator *)
        let res_percent = (make_char_ws '%') s index_to in
        parse_postfix (PerExpr e) index_from res_percent.index_to
      with X_no_match ->
        try
          (* Handle other postfix operators *)
          let res_op = nt_postfix_op s index_to in
          let e1 = res_op.found e in
          parse_postfix e1 index_from res_op.index_to
        with X_no_match ->
          { index_from = index_from; index_to = index_to; found = e }
    in
    parse_postfix res_atomic.found res_atomic.index_from res_atomic.index_to

  (* Parser for unary operators *)
  and nt_unary_op s i =
    maybe
      (disj
         (pack (make_char_ws '-') (fun _ -> '-'))
         (pack (make_char_ws '/') (fun _ -> '/')))
      s i

  and nt_unary s i =
    let nt =
      pack (caten nt_unary_op nt_postfix)
           (fun (op_opt, e) ->
              match op_opt with
              | Some '-' ->
                  (match e with
                   | Num n -> Num (-n)
                   | PerExpr (Num n) -> PerExpr (Num (-n))
                   | _ -> BinOp (Sub, Num 0, e))
              | Some '/' -> BinOp (Div, Num 1, e)
              | None -> e
              | Some _ -> e)
    in nt s i

  (* Parser for exponentiation (right-associative) *)
  and nt_power s i =
    chainr1 nt_unary nt_pow_op s i

  and nt_pow_op s i =
    let nt = pack (make_char_ws '^') (fun _ -> fun e1 e2 -> BinOp (Pow, e1, e2)) in
    nt s i

  (* Percentage addition and subtraction operators *)
  and nt_perc_add_sub_op s i =
    disj nt_add_per_op nt_sub_per_op s i

  and nt_add_per_op s i =
    pack (make_char_ws '+')
      (fun _ -> fun e1 e2 ->
         match e2 with
         | PerExpr e -> BinOp (AddPer, e1, e)
         | _ -> raise X_no_match) s i

  and nt_sub_per_op s i =
    pack (make_char_ws '-')
      (fun _ -> fun e1 e2 ->
         match e2 with
         | PerExpr e -> BinOp (SubPer, e1, e)
         | _ -> raise X_no_match) s i

  (* Multiplicative operators *)
  and nt_mul_op s i =
    pack (make_char_ws '*')
      (fun _ -> fun e1 e2 ->
         match e2 with
         | PerExpr e -> BinOp (PerOf, e1, e)
         | _ -> BinOp (Mul, e1, e2)) s i

  and nt_div_op s i =
    pack (make_char_ws '/')
      (fun _ -> fun e1 e2 -> BinOp (Div, e1, e2)) s i

  (* Modulo operator *)
  and nt_mod_op s i =
    pack (make_word_ws "mod")
      (fun _ -> fun e1 e2 -> BinOp (Mod, e1, e2)) s i

  (* Updated multiplicative operators *)
  and nt_mul_div_mod_op s i =
    disj_list [nt_mul_op; nt_div_op; nt_mod_op] s i

  (* Additive operators *)
  and nt_add_op s i =
    pack (make_char_ws '+')
      (fun _ -> fun e1 e2 -> BinOp (Add, e1, e2)) s i

  and nt_sub_op s i =
    pack (make_char_ws '-')
      (fun _ -> fun e1 e2 -> BinOp (Sub, e1, e2)) s i

  and nt_add_sub_op s i =
    disj nt_add_op nt_sub_op s i

  (* Parsing functions *)
  and nt_expr s i = nt_arith_expr s i

  and nt_arith_expr s i =
    chainl1 nt_term nt_add_sub_op s i

  and nt_term s i =
    chainl1 nt_factor nt_mul_div_mod_op s i

  and nt_factor s i =
    chainl1 nt_power nt_perc_add_sub_op s i

  (* Helper functions *)
  and chainl1 nt op s i =
    let rec parse_rest res_e =
      try
        let res_op = op s res_e.index_to in
        let res_e2 = nt s res_op.index_to in
        let new_e = res_op.found res_e.found res_e2.found in
        parse_rest { index_from = res_e.index_from; index_to = res_e2.index_to; found = new_e }
      with X_no_match -> res_e
    in
    let res_e = nt s i in
    parse_rest res_e

  and chainr1 nt op s i =
    let res_e1 = nt s i in
    try
      let res_op = op s res_e1.index_to in
      let res_e2 = chainr1 nt op s res_op.index_to in
      let new_e = res_op.found res_e1.found res_e2.found in
      { index_from = res_e1.index_from; index_to = res_e2.index_to; found = new_e }
    with X_no_match -> res_e1
  ;;
end;;

open InfixParser;;
