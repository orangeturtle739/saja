open Data
open Maybe

type t = {
  session_id: session_id;
  online_users: online_user list;
  messages: (username * message) list
}

let info state =
  List.map (fun {user={username;public_key=_}; ip_address} ->
      (username, ip_address)) state.online_users

let create users =
  {
    session_id =  Crypto.gen_session_id ();
    online_users = users;
    messages = [];
  }

let join session_id users =
  {
    session_id = Crypto.advance session_id;
    online_users = users;
    messages = [];
  }

let check_join session_id users =
  Crypto.verify_session_id session_id >>>| fun session_id ->
  (session_id, users)

let maybe_eq a b = if a = b then Some b else None

let receive_msg from session_id msg state =
  maybe_eq state.session_id session_id >>>| fun _ ->
  { state with messages = (from.user.username, msg)::state.messages; }

let send_msg my_name msg state =
  let id_ip_list = List.map (fun {user={username; public_key=_}; ip_address} ->
      (state.session_id, username, ip_address)) state.online_users in
  ({ state with messages = (my_name, msg)::state.messages; }, id_ip_list)

let send_init my_name state =
  let users = state.online_users in
  let ip_list = List.map (fun online_user -> online_user.ip_address) users in
  let hash_list = List.map (fun online_user ->
      Crypto.fingerprint online_user.user.public_key) users in
  let ip_fp_list = List.combine ip_list hash_list in
  let (new_state, dest_spec) = send_msg my_name "init" state in
  (Message.Init ip_fp_list,
   { new_state with session_id = Crypto.advance state.session_id; },
   dest_spec)
