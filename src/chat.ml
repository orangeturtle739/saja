open Data
open Maybe

type t = {
  online_users: (online_user * (session_id * session_id)) list;
  messages: (username * message) list
}

let create users =
  let initial_ids = List.map (fun _ -> Crypto.gen_session_id ()) users in
  {
    online_users = List.combine users (List.combine initial_ids initial_ids);
    messages = [];
  }

let join session_id users =
  {
    online_users = List.combine users (List.map (fun _ ->
        (session_id, Crypto.advance session_id)) users);
    messages = [];
  }

let rec lookup_user target = function
  | [] -> None
  | (user, session_ids)::_ when target = user -> Some session_ids
  | _::t -> lookup_user target t

let with_id user id users =
  let clear = List.remove_assoc user users in
  (user, id)::clear

let maybe_eq a b = if a = b then Some b else None

let receive_msg from session_id msg state =
  lookup_user from state.online_users >>>= fun (outgoing, incoming) ->
  maybe_eq incoming session_id >>>| fun incoming ->
  {
    online_users =
      with_id from (outgoing, Crypto.advance incoming) state.online_users;
    messages = (from.user.username, msg)::state.messages;
  }

let send_msg my_name msg state =
  let id_ip_list = List.map (fun ({user={username; public_key=_}; ip_address}, (outgoing, _)) ->
      (ip_address, username, outgoing)) state.online_users in
  ({
    online_users = List.map (fun (user, (outgoing, incoming)) ->
        (user, (Crypto.advance outgoing, incoming))) state.online_users;
    messages = (my_name, msg)::state.messages;
  }, id_ip_list)

let send_init my_name state =
  let users = state.online_users |> List.split |> fst in
  let ip_list = List.map (fun online_user -> online_user.ip_address) users in
  let hash_list = List.map (fun online_user ->
      Crypto.fingerprint online_user.user.public_key) users in
  let ip_fp_list = List.combine ip_list hash_list in
  let (new_state, dest_spec) = send_msg my_name "init" state in
  (Message.Init ip_fp_list, new_state, dest_spec)
