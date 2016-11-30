open Data
open Msgtransport
open Console
open Async.Std
open Keypersist


let chat_port = 12999
let init_str = "init"
let msg_str = "msg"

let null_key = {
  full_signing_key={n="0";e="0";d="0"};
  full_encryption_key={n="0";e="0";d="0"}
}

type chat_state = {
  online_users: (session_id * online_user) list;
  messages: (username * message) list
}

(* [action] represents an action taken. *)
type action =
  | Discover
  | StartSession of (username list)
  | QuitProgram
  | Help
  | SendMessage of message
  | GetInfo
  | ExitSession

(* [program state] is a representation type containing the relevant
   details of the program's state. *)
type program_state = {
  keys: Keypersist.t;
  user_ips: (username * ip_address) list;
  current_chat: chat_state option;
}

let handle_discovery state =
  let found = ref [] in
  Discovery.bind_discovery
    (fun online_user -> found := online_user::(!found));
  let add_user {user={username;public_key}; ip_address} state =
    let ok =
      if username = (Keypersist.retrieve_username state.keys) then return false
      else if Keypersist.verify_key username public_key state.keys then
        (printf_system "Discovered @%s at %s\n" username ip_address; return true) else
      if Keypersist.user_stored username state.keys then
        (print_system "*******************************\n";
         printf_system "Warning! There is something fishy about the key for @%s\n" username;
         print_system "The key stored in your keychain has a different fingerprint than\n";
         print_system "the key received:\n";
         printf_system "Keychain: %s\n"
           (Keypersist.retrieve_key username state.keys |> Crypto.fingerprint);
         printf_system "Received: %s\n"
           (Crypto.fingerprint public_key);
         print_system "Would you like to reject the received key? [y/n]\n";
         read_yes_no () >>| not) else
        (print_system "*******************************";
         printf_system "Warning! There is no key in your keychain for @%s\n" username;
         printf_system "Fingerprint: %s\n"
           (Crypto.fingerprint public_key);
         print_system "You should verify the fingerprint in person before accepting this key\n";
         print_system "Would you like to accept the received key? [y/n]\n";
         read_yes_no ()) in
    ok >>| (fun really_ok ->
        if not really_ok then state else
          {
            state with
            user_ips = (username, ip_address)::
                       (List.remove_assoc username state.user_ips);
            keys = write_key username public_key state.keys
          }
      ) in
  let rec process_users state = match !found with
    | [] -> return state
    | h::t -> found := t; add_user h state >>= process_users in
  Discovery.send_broadcast () >>= (fun sent ->
      if sent then (
        print_system "Sent broadcast.\n";
        after (Core.Std.sec 1.) >>= (fun _ ->
            (* After a second, stop collecting users *)
            Discovery.bind_discovery (fun _ -> ());
            process_users state))
      else
        (print_system "Error sending broadcast.\n"; return state))

let find_session chat_state target_username =
  chat_state.online_users |>
  List.find (fun (id, {user={username; public_key=_}; ip_address}) -> target_username = username)

let unwrap = function
  | Some thing -> thing
  | None -> failwith "Expected some"

let full_key_to_private {n;e;d} = {n;d}

let send_message state msg_type message username =
  let chat_state = state.current_chat |> unwrap in
  let (session, online_user) = find_session chat_state username in
  let next_session = Crypto.advance session in
  let full_message = next_session^"\n"^msg_type^"\n"^message in
  let key = Keypersist.retrieve_key username state.keys in
  let signing = Keypersist.retrieve_user_key state.keys in
  let encr_message =
    Crypto.encrypt key.encryption_key (signing.full_signing_key |> full_key_to_private) full_message in
  Msgtransport.send_msg online_user.ip_address chat_port full_message >>| (fun s ->
      let new_user_state = (next_session, online_user) in
      let new_user_map = chat_state.online_users |>
                         List.remove_assoc session in
      let new_chat_state =
        {online_users = (next_session, online_user)::new_user_map; messages = (username, message)::chat_state.messages} in
      {state with current_chat = Some new_chat_state}
    )

let send_group_messages state msg_type message_list =
  let rec send_to_users state = function
    | [] -> return state
    | (user, message)::t ->
      send_message state msg_type message user >>= (fun next_state ->
          send_to_users next_state t) in
  let username_list =
    state.current_chat |> unwrap |> (fun x -> x.online_users) |> List.split |>
    snd |> List.map (fun online_user -> online_user.user.username) in
  send_to_users state (List.combine username_list message_list)

let send_group_message state msg_type message =
  state.current_chat |> unwrap |> (fun x -> x.online_users) |>
  List.map (fun _ -> message) |> send_group_messages state msg_type

let (>>>=) m f = match m with
  | Some thing -> f thing
  | None -> None
let (>>>|) m f =
  m >>>= (fun thing -> Some (f thing))

let resolve_user state username =
  (if List.mem_assoc username state.user_ips then
     Some (List.assoc username state.user_ips) else None) >>>| fun ip ->
  {
    user =
      {
        username = username;
        public_key = Keypersist.retrieve_key username state.keys;
      };
    ip_address = ip
  }


let rec resolve_users state = function
  | [] -> Some []
  | h::t -> resolve_user state h >>>= fun resolved ->
    resolve_users state t >>>| fun rt ->
    resolved::rt

let start_session state username_list =
  match resolve_users state username_list with
  | Some users ->
    let initial_ids = List.map (fun _ -> Crypto.gen_session_id ()) users in
    let chat = {
      online_users = List.combine initial_ids users;
      messages = []
    } in
    let ip_list = List.map (fun online_user -> online_user.ip_address) users in
    let hash_list = List.map (fun online_user ->
        Crypto.fingerprint online_user.user.public_key) users in
    let init_msg = List.rev_map2 (fun a b -> a^" "^b) ip_list hash_list |>
                   String.concat "\n" in
    let new_state = {state with current_chat = Some chat} in
    send_group_message new_state init_str init_msg
  | None -> return state

let message_buf = Bqueue.create ()

let handle_incoming_message addr str =
  (* printf_system "Received: %s\nFound: %s" str addr; *)
  Bqueue.add (addr, str) message_buf

let resolve_init_body state body =
  let split = Str.split (Str.regexp "\n") body |>
              List.map (Str.split (Str.regexp " ")) |> (List.map (function
      | ip::fingerprint::[] -> (ip, fingerprint)
      | _ -> ("", ""))) in
  let my_fingerprint =
    Crypto.fingerprint_f (Keypersist.retrieve_user_key state.keys) in
  let good_split = List.filter (fun (_, fp) -> fp <> my_fingerprint) split in
  let chat_users =
    List.map (fun (ip,_) ->
        List.find (fun (_,rip) -> ip=rip) state.user_ips) good_split |>
    List.map fst |> resolve_users state in
  chat_users >>>| List.combine good_split >>>= fun user_data_lst ->
  let good_list =
    List.for_all
      (fun ((gip, gfp), {ip_address; user={username; public_key}}) ->
         gip = ip_address && gfp = (Crypto.fingerprint public_key))
      user_data_lst
  in
  if good_list then Some (List.split user_data_lst |> snd) else None


let process_init_message state addr session_id body =
  if state.current_chat <> None then return state else (
    match resolve_init_body state body with
    | Some chat_users ->
      print_system "You have been invited to a chat with: ";
      List.map (fun user ->
          printf_system "  * %s (%s)\n" user.user.username user.ip_address)
        chat_users |> ignore;
      print_system "\nWould you like to join the chat? [y/n]";
      read_yes_no () >>= fun join ->
      if join then (
        print_system "Joining chat.";
        return
          {
            state with
            current_chat= Some
                {
                  online_users =
                    List.combine (List.map (fun _ -> session_id) chat_users) chat_users;
                  messages = [];
                }
          }
      ) else return state
    | None -> print_system "Ignoring invitation.";
      return state )

let parse_message state msg =
  let split = Str.bounded_split (Str.regexp "\n") msg 3 in
  match split with
  | session_id::msg_type::body::[] -> Some (session_id, msg_type, body)
  | _ -> None

let handle_received_message state addr str =
  match parse_message state str with
  | Some (session_id, msg_type, body) when msg_type = init_str ->
    process_init_message state addr session_id body
  | _ -> failwith "process message unimplemented"

(* [execute] takes an action and a program state and returns
   a new program state with the action executed. *)
let execute (command: action) (state: program_state) : program_state Deferred.t =
  match command with
  | Discover -> handle_discovery state
  | StartSession user_lst -> start_session state user_lst
  | QuitProgram -> print_normal ">>|\n"; Async.Std.exit(0)
  | Help ->
    print_system
      ("---------------\n"^
       "Saja (adj) - Wise, sensible. [Ido language]\n"^
       "SAJA is an encrypted, peer-to-peer messaging system.\n"^
       "It was developed by four Cornell University students for a computer science course project.\n\n"^
       "COMMANDS:\n"^
       ":help -> Displays help/about window.\n"^
       ":quit -> Exits the program.\n"^
       ":discover -> Runs the UDP discovery module to find other users in the network.\n"^
       ":startsession <user1> <user2> ... <usern> -> Begins a session with n users with the specified usernames.\n"^
       ":info -> Gets information about the current session."^
       ":exitsession -> Exits the messaging session (but not the program). \n\n"^
       "If no command is specified, SAJA assumes you are trying to send a message and will attempt to send it.\n"
      );
    return state
  | SendMessage msg -> failwith "Unimplemented"
  | GetInfo -> failwith "Unimplemented"
  | ExitSession -> failwith "Unimplemented"

let action_of_string (s: string) : action =
  let tokens = Str.split (Str.regexp " ") s in
  let ntokens = List.filter (fun a -> if a<>"" then true else false) tokens in
  match ntokens with
  | [":discover"] -> Discover
  | [":quit"] -> QuitProgram
  | [":help"] -> Help
  | [":info"] -> GetInfo
  | [":exitsession"] -> ExitSession
  | ":startsession"::t -> StartSession t
  | _ -> SendMessage s

let rec main program_state =
  print_normal ">>= ";
  choose
    [
      choice (Bqueue.take message_buf) (fun (addr, str) ->
          `ReadMsg (addr, str));
      choice (Console.read_input ()) (fun str ->
          `ReadConsole str)
    ] >>= fun pick ->
  (match pick with
   | `ReadMsg (addr, str) -> handle_received_message program_state addr str
   | `ReadConsole s -> execute (action_of_string s) program_state)
  >>= fun new_state ->
  main new_state

let _ =
  print_normal
    (Logo.program_name^"\n");
  print_system "Welcome to SAJA (Siddant, Alex, Jacob, Amit) version 1.0.0.\n";
  print_system "Psst. You new around here? Type :help for help.\n";
  let _ = listen chat_port handle_incoming_message in
  let keys = Keypersist.load_keystore "password" in
  let keys = if Keypersist.retrieve_user_key keys = null_key then
      (print_system "Generating a fresh key pair.\n";
       let new_key = Crypto.gen_keys () in Keypersist.write_user_key new_key keys)
    else keys in
  let keys = (if Keypersist.retrieve_username keys = "" then
                (print_system "Messaging is more fun when people know your name. What's your name?\n";
                 read_input() >>= (fun new_user ->
                     let okay_message = "Alrighty! We'll call you " ^ new_user ^ ".\n" in
                     printf_system "%s" okay_message;
                     return (Keypersist.write_username new_user keys)))
              else (return keys)) >>= (fun keys ->
      Discovery.start_listening ();
      let user_key = Keypersist.retrieve_user_key keys in
      Discovery.set_key {
        username = Keypersist.retrieve_username keys;
        public_key = {
          encryption_key = {
            n = user_key.full_encryption_key.n;
            e = user_key.full_encryption_key.e
          };
          signing_key = {
            n = user_key.full_signing_key.n;
            e = user_key.full_signing_key.e
          }
        }
      };
      return keys) >>| (fun keys -> main
                           {
                             keys=keys;
                             user_ips = [];
                             current_chat = None;
                           }) in
  let _ = Signal.handle [Signal.of_string "sigint"]
      (fun _ -> print_system "\nBye!\n"; ignore (Async.Std.exit(0));) in
  let _ = Scheduler.go() in ()
