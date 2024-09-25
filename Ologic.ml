type expr =
  |Str_lit of string
  |Int_lit of int
  |Bool_lit of bool
;;

type test =
  |Eq of expr*expr
  |Neq of expr*expr
  |Gt of expr*expr
  |Lt of expr*expr
;;

type log_op = And | Or
;;

type cond =
  |True
  |False
  |Test of test
  |Cond of test*log_op*test
  |Conds of cond*log_op*cond
;;

(* TODO - cannot meaningfully compare literals of different types.
Default behaviour defined here as giving false.
Will make this function return a bool option and give None when different types are compared.
Can use this to through an exception in the main evaluation function later on.

Will need to consider functions which depend on comp_expr later
  -> eval_test directly uses comp_expr
  -> eval_op_test depends on eval_test
  -> eval_cond depends on eval_test
*)

let comp_expr bin_op e1 e2 =
  match (e1, e2) with
  |(Str_lit(s1), Str_lit(s2)) -> Some(bin_op e1 e2)
  |(Int_lit(n1), Int_lit(n2)) -> Some(bin_op e1 e2)
  |(Str_lit(s1), Int_lit(n1)) -> None
  |(Int_lit(n1), Str_lit(s2)) -> None
;;

let eval_test t =
  match t with
  |Eq(expr1, expr2) -> comp_expr ( = ) expr1 expr2
  |Neq(expr1, expr2) -> not(comp_expr ( = ) expr1 expr2)
  |Gt(expr1, expr2) -> comp_expr ( > ) expr1 expr2
  |Lt(expr1, expr2) -> comp_expr ( < ) expr1 expr2
;;

let eval_op_test o t1 t2 =
  let bin_op =
    if o = And then ( && )
    else ( || ) in
    try bin_op (eval_test t1) (eval_test t2) with
    |Failure(x) -> raise(Failure("Cannot compare expressions of different types"))
;;

let rec eval_cond c =
  match c with
  |True -> true
  |False -> false
  |Test(t) -> eval_test t
  |Cond(t1, o, t2)-> eval_op_test o t1 t2
  |Conds(c1, o, c2) ->
      if o = And then (eval_cond c1) && (eval_cond c2)
      else (eval_cond c1) || (eval_cond c2)
;;
