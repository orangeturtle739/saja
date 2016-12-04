open Maybe

type body = Msg of string | Init of (string * string) list | Join | Exit
type message = string * body

let msg_id = "msg"
let init_id = "init"
let join_id = "join"
let exit_id = "exit"

let msg_str = function
  | Msg _ -> msg_id
  | Init _ -> init_id
  | Join -> join_id
  | Exit -> exit_id

let body_to_string = function
  | Msg str -> [str]
  | Init lst -> (List.map (fun (a, b) -> a^" "^b) lst)
  | Join | Exit -> []

let to_string (session_id, body) =
  session_id::(msg_str body)::(body_to_string body) |>
  String.concat "\n"

let pair_split delim str = Str.bounded_split (Str.regexp delim) str 2

let pairify = function
  | [a; b] -> Some (a, b)
  | [a] -> Some (a, "")
  | _ -> None

let parse_init_body body =
  Str.split (Str.regexp "\n") body |> List.map (pair_split " ") |>
  map_m pairify

let body_from_string str =
  pair_split "\n" str |>
  pairify >>>= fun (msg_type, body) ->
  if msg_type = init_id then parse_init_body body >>>| (fun ibody ->
      Init ibody)
  else if msg_type = msg_id then Some (Msg body)
  else if msg_type = join_id then Some Join
  else if msg_type = exit_id then Some Exit
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
