type literal =
  |Str_lit of string
  |Int_lit of int
  |Bool_lit of bool
;;

type var =
  |Var of string*literal
;;

type expr =
  |Let of string*literal
  |Reassign of string*literal*literal
;;

type test =
  |Eq of literal*literal
  |Neq of literal*literal
  |Gt of literal*literal
  |Lt of literal*literal
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

type if_stat =
  |Body of expr list
  |If of cond*if_stat
  |Else of if_stat
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

let comp_lit bin_op l1 l2 =
  match (l1, l2) with
  |(Str_lit(s1), Str_lit(s2)) -> Some(bin_op l1 l2)
  |(Int_lit(n1), Int_lit(n2)) -> Some(bin_op l1 l2)
  |(Bool_lit(b1), Bool_lit(b2)) -> Some(bin_op l1 l2)
  |_ -> None
;;

let eval_test t =
  match t with
  |Eq(expr1, expr2) -> comp_lit ( = ) expr1 expr2
  |Neq(expr1, expr2) -> comp_lit ( != ) expr1 expr2
  |Gt(expr1, expr2) -> comp_lit ( > ) expr1 expr2
  |Lt(expr1, expr2) -> comp_lit ( < ) expr1 expr2
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
  |None -> raise(Failure("Invalid condition"))
  |Some(v) -> v
;;

let eval_expr l =
  match l with
  |Let(v_name, value) -> Var(v_name, value)
  |Reassign(v_name, old_value, new_value) -> Var(v_name, new_value)
;;