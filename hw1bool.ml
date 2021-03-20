(* Part 3 *)

(* Question 1 *)

type bool_expr =
    | Lit of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr
;;

let possible_inputs = [(true, true); (true, false); (false, true); (false, false)]
;;

(* Returns a true/false for the entire expression *)
let rec eval_bool a b booltup inexp =
    match inexp with
        |Lit x1 when x1 = a -> fst booltup
        |Lit x2 when x2 = b -> snd booltup
        |Lit _ -> false (*Shouldn't ever come up*)
        |Not subexp -> not (eval_bool a b booltup subexp)
        |And (sub1, sub2) -> (eval_bool a b booltup sub1) && (eval_bool a b booltup sub2)
        |Or (sub1, sub2) -> (eval_bool a b booltup sub1) || (eval_bool a b booltup sub2)
;;

let rec check_all_inputs a b inexp blist retlist =
    if blist = [] then
        retlist
    else
        check_all_inputs a b inexp (List.tl blist) (List.append retlist
        [(fst (List.hd blist), snd (List.hd blist), eval_bool a b (List.hd blist) inexp)])
    ;;

let truth_table a b inexp =
    check_all_inputs a b inexp possible_inputs []
;;
