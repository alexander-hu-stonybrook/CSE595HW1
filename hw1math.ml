(* Question 2 *)

type args = {arg1:expr ; arg2:expr}

and expr =
    | Const of int
    | Var of string
    | Plus of args
    | Minus of args
    | Mult of args
    | Div of args

;;

(* Question 3 *)

let rec eval_expr expr =
    match expr with
        |Const a -> a
        |Var _ -> 0 (*Will never come up because restrictions*)
        |Plus ag -> eval_expr(ag.arg1) + eval_expr(ag.arg2)
        |Minus ag -> eval_expr(ag.arg1) - eval_expr(ag.arg2)
        |Mult ag -> eval_expr(ag.arg1) * eval_expr(ag.arg2)
        |Div ag -> eval_expr(ag.arg1) / eval_expr(ag.arg2)
;;
