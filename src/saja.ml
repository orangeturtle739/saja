open Data
open Msgtransport
open Console
open Async.Std
open Keypersist
open Maybe

(* The port to do all message sending on *)
let chat_port = 12999
let found = ref []

let message_buf = Bqueue.create ()
let handler_buf = Bqueue.create ()

let null_key = {
  full_signing_key={n="0";e="0";d="0"};
  full_encryption_key={n="0";e="0";d="0"}
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
  | TransmitKeys of ip_address
  | ProcessUsers

(* [program state] is a representation type containing the relevant
   details of the program's state. *)
type program_state = {
  keys: Keypersist.t;
  user_ips: (username * ip_address) list;
  current_chat: Chat.t option;
}

let transmit_keys state ip =
  Discovery.tcp_key_transmit ip >>= (fun sent ->
      (if sent then print_system "Sent key.\n" else
         print_error "There was a problem sending your key.\n"); return state)

let rec process_users state =
  let add_user {user={username;public_key}; ip_address} state =
    let ok =
      if username = (Keypersist.retrieve_username state.keys) then return false
      else if Keypersist.verify_key username public_key state.keys then
        (print_system "Discovered ";
         printf_username "@%s " username;
         printf_system "at %s\n" ip_address; return true) else
      if Keypersist.user_stored username state.keys then
        (print_system "*******************************\n";
         print_system "Warning! There is something fishy about the key for ";
         printf_username "@%s\n" username;
         print_system "The key stored in your keychain has a different fingerprint than\n";
         print_system "the key received:\n";
         printf_system "Keychain: %s\n"
           (Keypersist.retrieve_key username state.keys |> Crypto.fingerprint);
         printf_system "Received: %s\n"
           (Crypto.fingerprint public_key);
         print_system "Would you like to reject the received key? [y/n]\n";
         read_yes_no () >>| not) else
        (print_system "*******************************\n";
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
  match !found with
  | [] -> return state
  | h::t -> found := t; add_user h state >>= process_users

let handle_discovery state =
  Discovery.send_broadcast () >>= (fun sent ->
      if sent then (
        print_system "Sent broadcast.\n";
        after (Core.Std.sec 1.) >>= (fun _ ->
            (* After a second, process users *)
            process_users state >>=
            (fun state ->
               found := []; return state)
          ))
      else
        (print_system "Error sending broadcast.\n"; return state))

let full_key_to_private {n;e=_;d} = {n;d}

let send_message state session_id username ip_address message_body =
  let full_message = Message.create session_id message_body |>
                     Message.to_string in
  let key = Keypersist.retrieve_key username state.keys in
  let signing = Keypersist.retrieve_user_key state.keys in
  let encr_message =
    Crypto.encrypt key.encryption_key
      (signing.full_signing_key |> full_key_to_private) full_message in
  print_endline "About to send message";
  print_endline ip_address;
  Msgtransport.send_msg ip_address chat_port encr_message

let send_group_message state message_body dest_spec =
  List.map (fun (username, ip, session_id) ->
      send_message state session_id username ip message_body) dest_spec |>
  Deferred.all >>| List.for_all (fun x -> print_endline (if x then "you" else "foo"); x)

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

let resolve_users state users = map_m (resolve_user state) users

let start_session state username_list =
  match resolve_users state username_list with
  | Some users ->
    let (init_body, chat, dest_spec) =
      Chat.create users |>
      Chat.send_init (Keypersist.retrieve_username state.keys) in
    let new_state = {state with current_chat = Some chat} in
    send_group_message new_state init_body dest_spec >>= fun worked ->
    if worked then (print_system "Sent invites."; return new_state)
    else (print_system "Failed to start chat."; return state)
  | None -> print_system "Unable to resolve usernames"; return state

let handle_incoming_message addr str =
  (* printf_system "Received: %s\nFound: %s" str addr; *)
  Bqueue.add (addr, str) message_buf

let resolve_init_body state body =
  let my_fingerprint =
    Crypto.fingerprint_f (Keypersist.retrieve_user_key state.keys) in
  let good_split = List.filter (fun (_, fp) -> fp <> my_fingerprint) body in
  let chat_users =
    List.map (fun (ip,_) ->
        List.find (fun (_,rip) -> ip=rip) state.user_ips) good_split |>
    List.map fst |> resolve_users state in
  chat_users >>>| List.combine good_split >>>= fun user_data_lst ->
  let good_list =
    List.for_all
      (fun ((gip, gfp), {ip_address; user={username=_; public_key}}) ->
         gip = ip_address && gfp = (Crypto.fingerprint public_key))
      user_data_lst
  in
  if good_list then Some (List.split user_data_lst |> snd) else None


let process_init_message state origin_user session_id body =
  match resolve_init_body state body with
  | Some chat_users ->
    let full_chat_users = origin_user::chat_users in
    print_system "You have been invited to a chat with: \n";
    List.map (fun user ->
        printf_username "  * @%s (%s)\n" user.user.username user.ip_address)
      full_chat_users |> ignore;
    print_system "Would you like to join the chat? [y/n]\n";
    read_yes_no () >>= fun join ->
    if join then (
      let my_name = Keypersist.retrieve_username state.keys in
      let joining_message = my_name^" joined." in
      let (chat, dest_spec) = Chat.join session_id full_chat_users |>
                              Chat.send_msg my_name joining_message in
      send_group_message
        state (Message.Msg joining_message) dest_spec >>= fun worked ->
      if not worked then (print_error "Unable to join chat"; return state)
      else (print_system "Joined chat.\n";
            return {state with current_chat = Some chat})
    ) else return state
  | None -> return state

let process_msg_messsage state from session_id body =
  let received = state.current_chat >>>=
    Chat.receive_msg from session_id body in
  match received with
  | None -> return state
  | Some chat_state ->
    printf_username "\n@%s: " from.user.username;
    printf_message "%s\n" body;
    return
      {
        state with
        current_chat = Some chat_state;
      }

let decrypt_message state str =
  let public_key_map = Keypersist.retrieve_keys state.keys in
  let public_signing_keys = List.split public_key_map |> snd |>
                            List.map (fun x -> x.signing_key) in
  let decryption_key =
    (Keypersist.retrieve_user_key state.keys).full_encryption_key |>
    full_key_to_private in
  Crypto.decrypt
    public_signing_keys decryption_key str >>>| fun (decrypted, signing_key) ->
  let signing_key_to_username_map = List.combine public_signing_keys
      (List.split public_key_map |> fst) in
  let username = List.assoc signing_key signing_key_to_username_map in
  (* printf_system "Received: %s\nFrom: %s\n" decrypted username; *)
  (username, decrypted)

let process_message state origin_user message =
  let session_id = (Message.session_id message) in
  match Message.body message with
  | Message.Msg body -> process_msg_messsage state origin_user session_id body
  | Message.Init body -> process_init_message state origin_user session_id body

let handle_received_message state addr str =
  decrypt_message state str >>>| fun (username, decrypted_message) ->
  let origin_user =
    {
      ip_address = addr;
      user =
        {
          username = username;
          public_key = Keypersist.retrieve_key username state.keys;
        };
    } in
  match Message.from_string decrypted_message with
  | Some message -> process_message state origin_user message
  | None -> return state

let handle_received_message_ignore state addr str =
  match handle_received_message state addr str with
  | Some thing -> thing
  | None -> return state

let handle_send_message state msg =
  match state.current_chat with
  | None -> print_system "Can't send message because you are not in a chat.\n";
    return state
  | Some chat_state ->
    let (new_chat, dest_spec) =
      Chat.send_msg (Keypersist.retrieve_username state.keys) msg chat_state in
    send_group_message state (Message.Msg msg) dest_spec >>= fun worked ->
    if not worked then print_error "Unable to send message" else ();
    return {state with current_chat = Some new_chat}

(* [execute] takes an action and a program state and returns
   a new program state with the action executed. *)
let execute (command: action) (state: program_state) : program_state Deferred.t =
  match command with
  | Discover -> handle_discovery state
  | StartSession user_lst -> start_session state user_lst
  | QuitProgram ->
    print_system "Saving keystore...\n";
    Keypersist.save_keystore state.keys;
    print_normal ">>|\n"; Async.Std.exit(0)
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
       ":info -> Gets information about the current session.\n"^
       ":exitsession -> Exits the messaging session (but not the program). \n\n"^
       "If no command is specified, SAJA assumes you are trying to send a message and will attempt to send it.\n"
      );
    return state
  | SendMessage msg -> handle_send_message state msg
  | GetInfo -> failwith "Unimplemented"
  | ExitSession -> failwith "Unimplemented"
  | TransmitKeys ip -> transmit_keys state ip
  | ProcessUsers -> process_users state

let action_of_string (s: string) : action =
  let tokens = Str.split (Str.regexp " ") s in
  let ntokens = List.filter (fun a -> if a<>"" then true else false) tokens in
  match ntokens with
  | [":discover"] -> Discover
  | [":quit"] -> QuitProgram
  | [":help"] -> Help
  | [":process"] -> ProcessUsers
  | [":info"] -> GetInfo
  | [":exitsession"] -> ExitSession
  | ":transmit"::[ip] -> TransmitKeys ip
  | ":startsession"::t -> StartSession t
  | _ -> SendMessage s

let rec main program_state =
  print_normal ">>= ";
  choose
    [
      choice (Bqueue.recent_take message_buf) (fun (addr, str) ->
          `ReadMsg (addr, str));
      choice (read_input ()) (fun str ->
          `ReadConsole str);
      choice (Bqueue.recent_take handler_buf) (fun () ->
          `HandlerCalled)
    ] >>= fun pick ->
  (match pick with
   | `ReadMsg (addr, str) -> handle_received_message_ignore program_state addr str
   | `ReadConsole s -> execute (action_of_string s) program_state
   | `HandlerCalled ->
     print_system "\nSaving keystore.\n";
     Keypersist.save_keystore program_state.keys;
     Async.Std.exit(0))
  >>= fun new_state ->
  main new_state

let rec prompt_password () =
  print_system "Please enter your password. If this is your first time, type in your desired password.\n";
  choose
    [
      choice (read_input ()) (fun str ->
          `ReadPass str);
      choice (Bqueue.recent_take handler_buf) (fun () ->
          `HandlerCalled)
    ] >>= fun pick ->
  match pick with
  | `ReadPass password ->
    (try
       return (Keypersist.load_keystore password) >>=
       (fun keys -> if Keypersist.retrieve_user_key keys = null_key then
           (print_system "Generating a fresh key pair.\n";
            let new_key = Crypto.gen_keys () in return (Keypersist.write_user_key new_key keys))
         else return keys) >>=
       (fun keys ->
          let user = Keypersist.retrieve_username keys in
          if user = "" then
            (print_system "Messaging is more fun when people know your name. What's your name?\n";
             read_input() >>= (fun new_user ->
                 let okay_message = "Alrighty! We'll call you " ^ new_user ^ ".\n" in
                 printf_system "%s" okay_message;
                 return (Keypersist.write_username new_user keys)))
          else
            (print_system ("Welcome back, " ^ user ^ ".\n");
             return keys)) >>=
       (fun keys ->
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
          return keys) >>| (fun keys -> {
             keys=keys;
             user_ips = [];
             current_chat = None;
           })
     with
       Persistence.Bad_password ->
       print_system "Incorrect password!\n";
       prompt_password ())
  | `HandlerCalled ->
    print_system "\nBye!\n";
    Async.Std.exit(0)

let _ =
  print_normal
    (Logo.program_name^"\n");
  print_system "Welcome to SAJA (Siddant, Alex, Jacob, Amit) version 1.0.0.\n";
  print_system "Psst. You new around here? Type :help for help.\n";
  (Discovery.bind_discovery
     (fun online_user -> found := online_user::(!found)));
  let _ = listen chat_port handle_incoming_message in
  let _ = prompt_password() >>| (fun init_state -> main init_state) in
  let _ = Signal.handle [Signal.of_string "sigint"]
      ~f:(fun _ -> Bqueue.add () handler_buf) in
  let _ = Scheduler.go() in ()
