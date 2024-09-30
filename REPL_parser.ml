open ConsString

type keyword = Let | If | Else | While | And | Or;;

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
  match remove_leading_whitespace (ConsString.rev c_s) with
  |None -> None
  |Some(v) -> Some(ConsString.rev v)
;;

let remove_interal_whitespace c_s =
  let rec remove_internal_whitespace_worker c_s res append_to_temp temp =
    match c_s with
    |Nil -> ConsString.concat (rev res) (rev temp)
    |Cons(c, t) ->
      if append_to_temp = true then
        if c = '\"' then remove_internal_whitespace_worker t (rev (ConsString.concat (rev res) (rev temp))) false Nil
        else remove_internal_whitespace_worker t res true (Cons(c, temp))
      else
        if c = '\"' then remove_internal_whitespace_worker t res true temp
        else
          if c = ' ' then remove_internal_whitespace_worker t res false temp
          else remove_internal_whitespace_worker t (Cons(c, res)) false temp
    in remove_internal_whitespace_worker c_s Nil false Nil
;;

let next_n n c_s =
  let rec next_n_worker n count c_s res =
    if count = n then Some(rev res) else
      match c_s with
      |Nil -> None
      |Cons(c, t) ->
        match next_n_worker n (count+1) t (Cons(c, res)) with
        |None -> None
        |Some(v) -> Some(v)
  in next_n_worker n 0 c_s Nil
;;

let statement_type c_s =
  let aux n =
    match next_n n c_s with
    |None -> ""
    |Some(v) -> cons_string_to_string v
  in
  if (aux 3) = "let " then Some(Let)
  else if (aux 3) = "if(" then Some(If)
  else if (aux 5) = "else{" then Some(Else)
  else None
;;

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