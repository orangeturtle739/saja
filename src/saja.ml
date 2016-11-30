open Data
open Msgtransport
open Console
open Async.Std
open Keypersist


let chat_port = 12999

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
        (print_system "Discovered @%s at %s\n" username ip_address; return true) else
      if Keypersist.user_stored username state.keys then
        (print_system "*******************************\n";
         print_system "Warning! There is something fishy about the key for @%s\n" username;
         print_system "The key stored in your keychain has a different fingerprint than\n";
         print_system "the key received:\n";
         print_system "Keychain: %s\n"
           (Keypersist.retrieve_key username state.keys |> Crypto.fingerprint);
         print_system "Received: %s\n"
           (Crypto.fingerprint public_key);
         print_system "Would you like to reject the received key? [y/n]\n";
         read_yes_no () >>| not) else
        (print_system "*******************************";
         print_system "Warning! There is no key in your keychain for @%s\n" username;
         print_system "Fingerprint: %s\n"
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

let send_message state message username =
  let chat_state = state.current_chat |> unwrap in
  let (session, online_user) = find_session chat_state username in
  let next_session = Crypto.advance session in
  let full_message = next_session^"\n"^message in
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

let start_session users =
  let initial_ids = List.map (fun _ -> Crypto.gen_session_id ()) users in
  let chat = {
    online_users = List.combine initial_ids users;
    messages = []
  } in failwith "ahh"


(* [execute] takes an action and a program state and returns
   a new program state with the action executed. *)
let execute (command: action) (state: program_state) : program_state Deferred.t =
  match command with
  | Discover -> handle_discovery state
  | StartSession user_lst -> failwith "???"
  | QuitProgram -> print_endline ">>|"; Async.Std.exit(0)
  | Help ->
    print_endline
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
       "If no command is specified, SAJA assumes you are trying to send a message and will attempt to send it."
      );
    return state
  | SendMessage msg -> failwith "Unimplemented"
  | GetInfo -> failwith "Unimplemented"
  | ExitSession -> failwith "Unimplemented"

let action_of_string (s: string) : action =
  let tokens = Str.split (Str.regexp " ") s in
  match tokens with
  | [":discover"] -> Discover
  | [":quit"] -> QuitProgram
  | [":help"] -> Help
  | [":info"] -> GetInfo
  | [":exitsession"] -> ExitSession
  | ":startsession"::t -> StartSession t
  | _ -> SendMessage s

let rec main program_state =
  print_system ">>= ";
  Console.read_input () >>= fun s ->
  execute (action_of_string s) program_state >>= fun new_state ->
  main new_state

let _ =
  print_endline
    Logo.program_name;
  print_system "Welcome to SAJA (Siddant, Alex, Jacob, Amit) version 1.0.0.\n";
  print_system "Psst. You new around here? Type :help for help.\n";
  let _ = listen 12999 (fun addr str -> print_system "Received: %s\nFound: %s" str addr) in
  let keys = Keypersist.load_keystore () in
  let keys = if Keypersist.retrieve_user_key keys = null_key then
      (print_system "Generating a fresh key pair.";
       let new_key = Crypto.gen_keys () in Keypersist.write_user_key new_key keys)
    else keys in
  let keys = (if Keypersist.retrieve_username keys = "" then
                (print_system "Messaging is more fun when people know your name. What's your name?";
                 read_input() >>= (fun new_user ->
                     let okay_message = "Alrighty! We'll call you " ^ new_user ^ ".\n" in
                     print_system "%s" okay_message;
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
      return keys) >>| (fun keys -> main {keys=keys; user_ips = []; current_chat = None}) in
  let _ = Signal.handle [Signal.of_string "sigint"]
      (fun _ -> print_system "\nBye!\n"; ignore (Async.Std.exit(0));) in
  let _ = Scheduler.go() in ()
