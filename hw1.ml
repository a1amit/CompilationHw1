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

type args_or_index = Args of expr list | Index of expr;;

module type INFIX_PARSER = sig
  val nt_expr : expr PC.parser
end;; (* module type INFIX_PARSER *)

module InfixParser : INFIX_PARSER = struct
open PC;;

let nt_expr =
  raise
    (X_not_yet_implemented
       "You need to implement this parser!");;

end;; (* module InfixParser *)

open InfixParser;;
