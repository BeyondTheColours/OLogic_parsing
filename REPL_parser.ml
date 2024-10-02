open ConsString

type keyword = Let | If | Else;;

let bind o f =
  match o with
  |None -> None
  |Some(v) -> f v
;;

let ( >>= ) o f = bind o f
;;

let extract o =
  match o with
  |None -> (Nil, Nil)
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

let trim c_s = c_s |> remove_leading_whitespace >>= remove_trailing_whitespace
;;

let remove_interal_whitespace c_s =
  let rec remove_internal_whitespace_worker c_s res append_to_temp temp =
    match c_s with
    |Nil -> None
    |Cons(c, t) ->
      if append_to_temp = true || c = '\"' then
        match remove_internal_whitespace_worker t res true (Cons(c, temp)) with
        |None -> Some(concat (rev res) (rev (Cons(c, temp))))
        |Some(v) -> Some(v)
      else
        if c = ' ' then
          match remove_internal_whitespace_worker t res false temp with
          |None -> Some(concat (rev res) (rev temp))
          |Some(v) -> Some(v)
        else
          match remove_internal_whitespace_worker t (Cons(c, res)) false temp with
          |None -> Some(concat (rev (Cons(c, res))) (rev temp))
          |Some(v) -> Some(v)
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

let consume_n n c_s =
  let rec consume_n_worker n count c_s =
    if count = n then Some(c_s) else
      match c_s with
      |Nil -> None
      |Cons(c, t) ->
        match consume_n_worker n (count+1) t with
        |None -> None
        |Some(v) -> Some(v)
  in consume_n_worker n 0 c_s
;;

let statement_type c_s =
  let aux n =
    match next_n n c_s with
    |None -> ""
    |Some(v) -> cons_string_to_string v
  in
  if (aux 4) = "let " then Some(Let, c_s)
  else if (aux 3) = "if(" then Some(If, c_s)
  else if (aux 5) = "else{" then Some(Else, c_s)
  else None
;;

let parse_let let_stat =
  let rec split res stat =
    match stat with
    |Nil -> None
    |Cons(c, t) ->
      if c = '=' then (Some(res, t)) else
        match split (Cons(c, res)) t with
        |None -> None
        |Some(v) -> Some(v)
  in let remove_semicolon split_stat =
    match split_stat with
    |(a, b) ->
      let rev_b = rev b in
      match rev_b with
      |Cons(c, t) ->
        if c = ';' then Some(a, rev t)
        else None
      |Nil -> None
  in consume_n 4 let_stat >>= remove_interal_whitespace >>= split Nil >>= remove_semicolon
;;

let parse_statement stat_type_c_s_tuple =
  match stat_type_c_s_tuple with
  |(Let, c_s) -> parse_let c_s
  |(If, c_s) -> Some(c_s, c_s)(*consume_n 2 c_s >>= remove_interal_whitespace*)
  |(Else, c_s) -> Some(c_s, c_s)(*consume_n 4 c_s >>= remove_interal_whitespace*)
;;

let rec loop mem =
  print_string("~~>");
  let get_input =
    let input = ConsString.string_to_cons_string(read_line()) in
    input |> trim >>= statement_type >>= parse_statement in
  let extracted_input = extract get_input in
  if extracted_input = (Nil, Nil) then print_endline("Invalid input\n")
  else let output =
    match extracted_input with
    |(a,b) -> (cons_string_to_string a)^" "^(cons_string_to_string b)
     in
  print_endline(output^"\n");
  loop (output::mem)

;;

let go = loop [];;