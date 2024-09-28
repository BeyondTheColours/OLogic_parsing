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

type else_stat =
  |Else of expr list
;;

type if_stat =
  |If of cond*expr list*else_stat
;;

let log_op_to_bin_op log_op =
  match log_op with
  |And -> ( && )
  |Or -> ( || )
;;

let bind (o1, o2) f =
  match (o1, o2) with
  |(None, None) -> None 
  |(Some(v1), Some(v2)) -> f v1 v2
  |_ -> None
;;

let comp_opt bin_op a b = Some(bin_op a b);;

let ( >>= ) o f = bind o f;;

let comp_expr bin_op e1 e2 =
  match (e1, e2) with
  |(Str_lit(s1), Str_lit(s2)) -> Some(bin_op e1 e2)
  |(Int_lit(n1), Int_lit(n2)) -> Some(bin_op e1 e2)
  |(Bool_lit(b1), Bool_lit(b2)) -> Some(bin_op e1 e2)
  |_ -> None
;;

let eval_test t =
  match t with
  |Eq(expr1, expr2) -> comp_expr ( = ) expr1 expr2
  |Neq(expr1, expr2) -> comp_expr ( != ) expr1 expr2
  |Gt(expr1, expr2) -> comp_expr ( > ) expr1 expr2
  |Lt(expr1, expr2) -> comp_expr ( < ) expr1 expr2
;;

let eval_op_test o t1 t2 = ((eval_test t1), (eval_test t2)) >>= comp_opt(log_op_to_bin_op o)
;;

let rec eval_cond c =
  match c with
  |True -> Some(true)
  |False -> Some(false)
  |Test(t) -> eval_test t
  |Cond(t1, o, t2)-> eval_op_test o t1 t2
  |Conds(c1, o, c2) -> ((eval_cond c1), (eval_cond c2)) >>= comp_opt (log_op_to_bin_op o)
;;

let extract_evaluated_cond e =
  match e with
  |None -> raise(Failure("Invalid conditional statement"))
  |Some(v) -> v
;;
