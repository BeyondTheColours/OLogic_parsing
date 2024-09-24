type expr =
  |Str_lit of string
  |Int_lit of int
;;

type test =
  |Eq of expr*expr
  |Neq of expr*expr
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

let comp_expr e1 e2 =
  match (e1, e2) with
  |(Str_lit(s1), Str_lit(s2)) -> s1 = s2
  |(Int_lit(n1), Int_lit(n2)) -> n1 = n2
  |(Str_lit(s1), Int_lit(n1)) -> false
  |(Int_lit(n1), Str_lit(s2)) -> false
;;

let eval_test t =
  match t with
  |Eq(expr1, expr2) -> comp_expr expr1 expr2
  |Neq(expr1, expr2) -> not(comp_expr expr1 expr2)
;;

let eval_op_test o t1 t2 =
  match o with
  |And -> (eval_test t1 && eval_test t2)
  |Or -> (eval_test t1 || eval_test t2)

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
