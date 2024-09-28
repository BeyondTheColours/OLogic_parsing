open ConsString

let test = print_endline("Here");;

type keyword = Let | If |Else |While |AND |OR |Quit;;

let rec remove_leading_whitespace c_s =
  match c_s with
  |Nil -> None
  |Cons(c, t) ->
    if c != ' ' then Some(Cons(c, t)) else
    match remove_leading_whitespace t with
    |None -> None
    |Some(v) -> Some(v)
;;

let rec remove_trailing_whitespace c_s =
  match remove_leading_whitespace (ConsString.rev(c_s)) with
  |None -> None
  |Some(v) -> Some(ConsString.rev v)
;;

let bind o f =
  match o with
  |None -> None
  |Some(v) -> f v
;;

let ( >>= ) o f = bind o f
;;

let extract_input input =
  match input with
  |None -> Nil
  |Some(v) -> v
;;

let test = print_endline("I have made a change");;

while true do
  print_string("~~>");
  let get_input =
    let input = ConsString.string_to_cons_string(read_line()) in
    input |> remove_leading_whitespace >>= remove_trailing_whitespace in
  let extracted_input = extract_input get_input in
  if extracted_input = Nil then print_endline("Invalid input\n")
  else let output = ConsString.cons_string_to_string extracted_input in
  print_endline(output^"\n")
done