let bind (o1, o2) f =
  match (o1, o2) with
  |(None, None) -> None 
  |(Some(v1), Some(v2)) -> f v1 v2
  |_ -> None
;;

let f bin_op a b = Some(bin_op a b);;

let v1 = true
let v2 = false
;;

let o1 = Some(v1)
let o2 = Some(v2) 
;;

let ( >>= ) (a, b) f = bind (a, b) f;;

(* An example *)
let out = (o1, o2) >>= f (=);;
