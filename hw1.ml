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
   | Deref of expr * expr
   | Call of expr * expr list;;
 
 module type INFIX_PARSER = sig
   val nt_expr : expr PC.parser
 end;; (* module type INFIX_PARSER *)
 
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
 
   let nt_number =
     pack (caten nt_skip_whitespace nt_integer)
          (fun (_, n) -> Num n);;
 
   (* Parser for variables *)
   let nt_var_start = disj (range 'a' 'z') (range 'A' 'Z');;
   let nt_var_char =
     disj_list [nt_var_start; range '0' '9'; char '$'; char '_'];;
 
   let nt_var_name = plus nt_var_char;;
 
   let nt_variable =
     pack (caten nt_skip_whitespace nt_var_name)
          (fun (_, cs) -> Var (string_of_list cs));;
 
   (* Recursive parser for expressions *)
   let rec nt_expr s i = nt_percent_expr s i
 
   (* Parser for parenthesized expressions *)
   and nt_paren_expr s i =
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
 
   (* Parser for postfix operators (function calls and indexing) *)
   and nt_postfix_op s i =
     (disj nt_call_op nt_index_op) s i
 
   (* Parser for expressions with postfix operators *)
   and nt_postfix s i =
     let nt =
       pack (caten nt_atomic (star nt_postfix_op))
            (fun (e, ops) ->
               List.fold_left (fun acc op -> op acc) e ops)
     in nt s i
 
   (* Parser for unary operators *)
   and nt_unary_op s i =
     let nt =
       maybe
         (disj (pack (make_char_ws '-') (fun _ -> '-'))
               (pack (make_char_ws '/') (fun _ -> '/')))
     in nt s i
 
   (* Parser for expressions with unary operators *)
   and nt_unary s i =
     let nt =
       pack (caten nt_unary_op nt_postfix)
            (fun (op_opt, e) ->
               match op_opt with
               | Some '-' -> BinOp (Sub, Num 0, e)
               | Some '/' -> BinOp (Div, Num 1, e)
               | None -> e
               | Some _ -> raise X_no_match)
     in nt s i
 
   (* Parser for exponentiation (right-associative) *)
   and nt_power s i =
     let nt =
       pack (caten nt_unary (maybe (pack (caten (make_char_ws '^') nt_power) (fun (_, e) -> e))))
            (fun (e1, e2_opt) ->
               match e2_opt with
               | Some e2 -> BinOp (Pow, e1, e2)
               | None -> e1)
     in nt s i
 
   (* Parser for multiplicative operators *)
   and nt_mul_op s i =
     let nt =
       disj_list
         [ pack (make_word_ws "mod") (fun _ -> fun e1 e2 -> BinOp (Mod, e1, e2));
           pack (make_char_ws '*') (fun _ -> fun e1 e2 -> BinOp (Mul, e1, e2));
           pack (make_char_ws '/') (fun _ -> fun e1 e2 -> BinOp (Div, e1, e2)) ]
     in nt s i
 
   (* Parser for terms (multiplication, division, modulus) *)
   and nt_term s i =
     chainl1 nt_power nt_mul_op s i
 
   (* Parser for additive operators *)
   and nt_add_op s i =
     let nt =
       disj_list
         [ pack (make_char_ws '+') (fun _ -> fun e1 e2 -> BinOp (Add, e1, e2));
           pack (make_char_ws '-') (fun _ -> fun e1 e2 -> BinOp (Sub, e1, e2)) ]
     in nt s i
 
   (* Parser for arithmetic expressions (addition, subtraction) *)
   and nt_arith_expr s i =
     chainl1 nt_term nt_add_op s i
 
   (* Parser for percentage operators *)
   and nt_per_op s i =
     let nt =
       disj_list
         [ pack (make_word_ws "+%") (fun _ -> fun e1 e2 -> BinOp (AddPer, e1, e2));
           pack (make_word_ws "-%") (fun _ -> fun e1 e2 -> BinOp (SubPer, e1, e2));
           pack (make_word_ws "*%") (fun _ -> fun e1 e2 -> BinOp (PerOf, e1, e2)) ]
     in nt s i
 
   (* Parser for expressions with percentage operators *)
   and nt_percent_expr s i =
     chainl1 nt_arith_expr nt_per_op s i
 
   (* Helper function for left-associative parsing *)
   and chainl1 nt op s i =
     let { index_from = i_start; index_to = i1; found = e1 } = nt s i in
     let rec rest e i_last =
       try
         let { index_from = op_from; index_to = op_to; found = f } = op s i_last in
         let { index_from = nt_from; index_to = nt_to; found = e2 } = nt s op_to in
         let e' = f e e2 in
         rest e' nt_to
       with X_no_match -> { index_from = i_start; index_to = i_last; found = e }
     in
     rest e1 i1
   ;;
 end;; (* module InfixParser *)
 
 open InfixParser;;
 