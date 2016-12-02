open Maybe

type body = Msg of string | Init of (string*string) list
type message = string * body

let init_str = "init"
let msg_str = "msg"

let body_to_string = function
  | Msg str -> [msg_str; str]
  | Init lst -> init_str::(List.map (fun (a, b) -> a^" "^b) lst)

let to_string (session_id, body) = session_id::(body_to_string body) |>
                                   String.concat "\n"

let pair_split delim str = Str.bounded_split (Str.regexp delim) str 2

let pairify = function
  | [a; b] -> Some (a, b)
  | _ -> None

let parse_init_body body =
  Str.split (Str.regexp "\n") body |> List.map (pair_split " ") |>
  map_m pairify

let body_from_string str =
  pair_split "\n" str |>
  pairify >>>= fun (msg_type, body) ->
  if msg_type = init_str then parse_init_body body >>>| (fun ibody ->
      Init ibody)
  else if msg_type = msg_str then Some (Msg body)
  else None

let from_string str =
  Str.bounded_split (Str.regexp "\n") str 2 |>
  pairify >>>= fun (session_id, body) ->
  body_from_string body >>>| fun parsed_body ->
  (session_id, parsed_body)

let session_id (session_id, _) = session_id

let body (_, body) = body

let create session_id body = (session_id, body)
let msg session_id message = create session_id (Msg message)
let init session_id ip_fp_list = create session_id (Init ip_fp_list)
