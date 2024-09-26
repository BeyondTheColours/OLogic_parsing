open ConsString

type keyword = Let | If |Else |While |AND |OR |Quit;;

let rec remove_leading_whitespace c_s =
  match c_s with
  |Nil -> None
  |Cons(c, t) ->
    if c != ' ' then Some(Cons(c, t)) else
    match remove_leading_whitespace_worker t with
    |None -> None
    |Some(v) -> Some(v)
;;

let rec remove_trailing_whitespace c_s =
  ConsString.rev(remove_leading_whitespace ConsString.rev(c_s))
;;

let bind o f =
  match o with
  |None -> None
  |Some(v) -> f v
;;

let ( >>= ) o f = bind o f
;;

let get_input =
  let input = ConsString.string_to_cons_string(readline("~~>")) in
  let cleanedinput = input >>= remove_leading_whitespace >>= remove_trailing_whitespace in
  cleaned_input
;;

let extract_input input =
  match input with
  |None -> Nil
  |Some(v) -> v
;;

let rec loop =
  let input = get_input in
  let extracted_input = extract_input input in
  if extracted_input = Nil then
    let print_error = print_endline "Invalid input" in loop
  else
    let string_of_input = ConsString.cons_string_to_string extracted input in
    let print_input = print_endline string_of_int in
    loop
;;
