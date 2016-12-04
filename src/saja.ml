open Data
open Msgtransport
open Console
open Async.Std
open Keypersist
open Maybe

(* The port to do all message sending on *)
let chat_port = 12999
(* The buffer which stores the online users which we've found through
 * broadcasts *)
let found = ref []
(* The buffer for storing incoming messages *)
let message_buf = Bqueue.create ()
(* The buffer for storing SIGINT requests *)
let handler_buf = Bqueue.create ()

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
  | Fingerprint of username
  | FingerprintU
  | Keys
  | SaveChat of filename

(* [program state] is a representation type containing the relevant
   details of the program's state. *)
type program_state = {
  keys: Keypersist.t;
  user_ips: (username * ip_address) list;
  current_chat: Chat.t option;
}

(* Sends the user's key to the specified IP address *)
let transmit_keys ip =
  Discovery.tcp_key_transmit ip >>| fun sent ->
  if sent then print_system "Sent key.\n" else
    print_error "There was a problem sending your key.\n"

(* Safely adds the specified used to the keychain, prompting if needed
 * to verify the key fingerprint. *)
let add_user {user={username;public_key}; ip_address} state =
  (if username = (Keypersist.retrieve_username state.keys) then return false
   else if Keypersist.verify_key username public_key state.keys then
     (print_system "Discovered ";
      printf_username "@%s " username;
      printf_system "at %s\n" ip_address; return true) else
   if Keypersist.user_stored username state.keys then
     (print_system "*******************************\n";
      print_error "Warning! There is something fishy about the key for ";
      printf_username "@%s\n" username;
      print_system
        "The key stored in your keychain has a different fingerprint than\n";
      print_system "the key received:\n";
      printf_system "Keychain: %s\n"
        (Keypersist.retrieve_key username state.keys |> Crypto.fingerprint);
      printf_system "Received: %s\n"
        (Crypto.fingerprint public_key);
      print_system "Would you like to reject the received key? [y/n]\n";
      read_yes_no () >>| not) else
     (print_system "*******************************\n";
      printf_error
        "Warning! There is no key in your keychain for @%s\n" username;
      printf_system "Fingerprint: %s\n"
        (Crypto.fingerprint public_key);
      print_system
        "You should verify the fingerprint in person before accepting this key\n";
      print_system "Would you like to accept the received key? [y/n]\n";
      read_yes_no ())) >>| (function
      | true -> {
          state with
          user_ips = (username, ip_address)::
                     (List.remove_assoc username state.user_ips);
          keys = write_key username public_key state.keys
        }
      | false -> state)

(* Adds the specified users safely to the keychain *)
let rec add_users users state = match users with
  | [] -> return state
  | h::t -> add_user h state >>= add_users t

(* Processes the current users in found *)
let process_users state =
  let current_found = !found in
  found := [];
  add_users current_found state

(* Sends a broadcast and processes users *)
let handle_discovery state =
  Discovery.send_broadcast () >>= function
  | true ->
    print_system "Sent broadcast.\n";
    after (Core.Std.sec 1.) >>= fun _ ->
    (* After a second, process users *)
    process_users state
  | false -> print_system "Error sending broadcast.\n"; return state

(* Converts a full key to a private key *)
let full_key_to_private {n;e=_;d} = {n;d}

(* Sends a message with the specified session ID to the user user with
 * the specified username and ip address with the specified message body
 * returns: true if the message was sent *)
let send_message state session_id username ip_address message_body =
  let full_message = Message.create session_id message_body |>
                     Message.to_string in
  let key = Keypersist.retrieve_key username state.keys in
  let signing = Keypersist.retrieve_user_key state.keys in
  let encr_message =
    Crypto.encrypt key.encryption_key
      (signing.full_signing_key |> full_key_to_private) full_message in
  Msgtransport.send_msg ip_address chat_port encr_message

(* Sends the specified message to all the (session_id, username, ip) tuples
 * in dest_spec.
 * returns: true if all messages were sent *)
let send_group_message state message_body dest_spec =
  List.map (fun (session_id, username, ip) ->
      send_message state session_id username ip message_body) dest_spec |>
  Deferred.all >>| List.for_all (fun x -> x)

let option_assoc key assoc =
  try
    Some (List.assoc key assoc)
  with
    Not_found -> None

(* Tries to resolve a username to an online user.
 * returns: [Some user] if the username was valid and discovered,
 * [None] if not *)
let resolve_user state username =
  option_assoc username state.user_ips >>>| fun ip ->
  {
    user =
      {
        username = username;
        public_key = Keypersist.retrieve_key username state.keys;
      };
    ip_address = ip
  }

(* Tries to resolve a list of users.
 * returns: [Some lst] if it worked, [None] if there was a problem
 * with any user *)
let resolve_users state users = map_m (resolve_user state) users

(* Starts a session with the specified usernames *)
let start_session state username_list =
  match resolve_users state username_list with
  | Some [] -> print_system ("Please provide a list of usernames. For example,"
                             ^" ':startsession alice bob'\n"); return state
  | Some users ->
    let (init_body, chat, dest_spec) =
      Chat.create users |>
      Chat.send_init (Keypersist.retrieve_username state.keys) in
    let new_state = {state with current_chat = Some chat} in
    send_group_message new_state init_body dest_spec >>= fun worked ->
    if worked then (print_system "Sent invites.\n"; return new_state)
    else (print_system "Failed to start chat.\n"; return state)
  | None -> print_error "Unable to resolve usernames.\n"; return state

(* Adds the pair (addr, str) to the message buf *)
let handle_incoming_message addr str =
  Bqueue.add (addr, str) message_buf

(* Resolves the body of an init message into [Some online_users] or
 * [None] if the keys or IP address don't verify. Also removes the current
 * user from the list *)
let resolve_init_body state body =
  let my_fingerprint =
    Crypto.fingerprint_f (Keypersist.retrieve_user_key state.keys) in
  List.filter (fun (_, fp) -> fp <> my_fingerprint) body |> map_m (fun (ip, fp) ->
      Keypersist.retrieve_fingerprint_user fp state.keys >>>| fun username ->
      {
        user = {
          username;
          public_key = Keypersist.retrieve_key username state.keys;
        };
        ip_address = ip;
      }
    )

(* Processes an init message *)
let process_init_message state origin_user session_id body =
  match resolve_init_body state body >>>= Chat.check_join session_id with
  | Some (session_id, chat_users) ->
    let full_chat_users = origin_user::chat_users in
    print_system "You have been invited to a chat with: \n";
    List.map (fun user ->
        printf_username "  * @%s (%s)\n" user.user.username user.ip_address)
      full_chat_users |> ignore;
    print_system "Would you like to join the chat? [y/n]\n";
    read_yes_no () >>= fun join ->
    if join then (
      let my_name = Keypersist.retrieve_username state.keys in
      let joining_message = " joined." in
      let (chat, dest_spec) = Chat.join session_id full_chat_users |>
                              Chat.send_msg my_name joining_message in
      send_group_message
        state (Message.Msg joining_message) dest_spec >>= fun worked ->
      if not worked then (print_error "Unable to join chat\n"; return (false, return state))
      else (print_system "Joined chat.\n";
            return (true, return {state with current_chat = Some chat}))
    ) else return (true, return state)
  | None -> return (false, return state)

(* Processes an incoming message *)
let process_msg_messsage state from session_id body =
  let received = state.current_chat >>>=
    Chat.receive_msg from session_id body in
  match received with
  | None -> return state
  | Some chat_state ->
    printf_username "@%s: " from.user.username;
    printf_message "%s\n" body;
    return
      {
        state with
        current_chat = Some chat_state;
      }

(* Decrypts the message, returning [Some (username, decrypted)]
 * if the message decrypted properly. Otherwise, returns [None] *)
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
  (username, decrypted)

(* Processes any incoming message from the network *)
let process_message state origin_user message =
  let session_id = (Message.session_id message) in
  match Message.body message with
  | Message.Msg body -> return (false, process_msg_messsage state origin_user session_id body)
  | Message.Init body -> process_init_message state origin_user session_id body

(* Handles an incoming network message *)
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
  | None -> return (false, return state)

(* Handles a message, returning the current state if the message failed
 * to process, or the new state if the message processed succesfully. *)
let handle_received_message_ignore state addr str =
  match handle_received_message state addr str with
  | Some thing -> thing
  | None -> return (false, return state)

(* Tries to send a message in the current session *)
let handle_send_message state msg exit =
  match state.current_chat with
  | None -> print_error "Can't send message because you are not in a chat.\n";
    return state
  | Some chat_state ->
    let (new_chat, dest_spec) =
      Chat.send_msg (Keypersist.retrieve_username state.keys) msg chat_state in
    send_group_message state (Message.Msg msg) dest_spec >>= fun worked ->
    if exit then return {state with current_chat = Some new_chat}
    else (if not worked then print_error "Unable to send message\n" else ();
    return {state with current_chat = Some new_chat})

(* Tries to exit the current session *)
let exit_session state =
  match state.current_chat with
  | None -> print_error "Can't exit session because your are not in a session.\n";
    return state
  | Some _ ->
    let exit_message =
      " left the chat.\n" in
    handle_send_message state exit_message true >>= fun state ->
    print_system "Exited chat.\n";
    return {state with current_chat = None}

(* Prints info about the current session *)
let get_info state =
  match state.current_chat with
  | None -> print_system "No current chat.\n"
  | Some chat_state ->
    print_system "You are currently in a chat with:\n";
    Chat.info chat_state |>
    List.map (fun (username, ip)->
        printf_username "  * @%s (%s)\n" username ip) |>
    ignore

(* Prints the pawprint (fingerprint) for the (username, fingerprint) pair *)
let pawprint (u,f) =
  print_system "Fingerprint "; print_username ("@"^u^": ");
  print_normal (f^"\n")

(* Shows the fingerprint for the specified user *)
let process_fingerprint state user =
  if Keypersist.user_stored user state.keys then
    (user, Crypto.fingerprint (retrieve_key user state.keys)) |> pawprint
  else (print_error "No key stored for "; print_username ("@"^user); print_error "\n")
(* Prints the current user's fingerprint *)
let own_fingerprint state =
  (retrieve_username state.keys,
   Crypto.fingerprint_f(retrieve_user_key state.keys)) |> pawprint

(* Lists all known fingerprints *)
let list_keys state =
  Keypersist.retrieve_keys state.keys |> List.split |> fst |>
  List.map (process_fingerprint state) |> ignore

(* Saves a chat log to file. *)
let save_chat file state =
  match state.current_chat with
  | Some chat -> Msgpersist.write_log file (Chat.msg_log chat)
  | None -> print_error "No chat currently opened.\n"

(* Safely exits the program by leaving the current chat and saving the keystore *)
let safe_exit state =
  (match state.current_chat with
   | None -> return state
   | Some _ -> exit_session state)
  >>= fun state ->
  print_system "\nSaving keystore...\n";
  Keypersist.save_keystore state.keys;
  printf_prompt ">>|\n"; Async.Std.exit(0)

(* [execute] takes an action and a program state and returns
   a new program state with the action executed. *)
let execute (command: action) (state: program_state) : program_state Deferred.t =
  match command with
  | Discover -> handle_discovery state
  | StartSession user_lst -> start_session state user_lst
  | QuitProgram -> safe_exit state
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
       ":exitsession -> Exits the messaging session (but not the program). \n"^
       ":transmit <ip-addr> -> Manually transmits your public key to an IP address. \n"^
       ":process -> Processes any public keys that have been manually sent to you. \n"^
       ":fingerprint -> Shows your fingerprint.\n"^
       ":fingerprint <username> -> Shows the fingerprint of the user with the given username."^
       "If no command is specified, SAJA assumes you are trying to send a message and will attempt to send it.\n"^
       ":savechat <file> -> Saves the current chat log to file.\n"
      );
    return state
  | SendMessage msg when msg <> "" -> handle_send_message state msg false
  | SendMessage _ -> return state
  | GetInfo -> get_info state; return state
  | ExitSession -> exit_session state
  | TransmitKeys ip -> transmit_keys ip >>| fun _ -> state
  | ProcessUsers -> process_users state
  | Fingerprint u -> process_fingerprint state u; return state
  | FingerprintU -> own_fingerprint state; return state
  | Keys -> list_keys state; return state
  | SaveChat file -> save_chat file state; return state

(* Parses a string into an action *)
let action_of_string (s: string) : action =
  let tokens = Str.split (Str.regexp " ") s in
  let ntokens = List.filter (fun a -> a<>"") tokens in
  match ntokens with
  | [":discover"] -> Discover
  | [":quit"] -> QuitProgram
  | [":help"] -> Help
  | [":process"] -> ProcessUsers
  | [":info"] -> GetInfo
  | [":exitsession"] -> ExitSession
  | ":transmit"::[ip] -> TransmitKeys ip
  | ":startsession"::t -> StartSession t
  | [":fingerprint"] -> FingerprintU
  | ":fingerprint"::[u] -> Fingerprint u
  | [":keys"] -> Keys
  | [":savechat"; file] -> SaveChat file  
  | _ -> SendMessage s

(* Main loop *)
let rec main need_prompt program_state =
  if need_prompt then printf_prompt ">>= " else ();
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
   | `ReadConsole s -> return (true, execute (action_of_string s) program_state)
   | `HandlerCalled -> return (false, safe_exit program_state))
  >>= fun (need_prompt, program_state) -> program_state >>= 
  main need_prompt

(* Processes the keychain into the initial user state *)
let process_keys_to_init keys =
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
  {
    keys=keys;
    user_ips = [];
    current_chat = None;
  }

(* Prompts the user for a username *)
let rec prompt_username keys =
  let user = Keypersist.retrieve_username keys in
  if user = "" then begin
    print_system
      "Messaging is more fun when people know your name. What's your name?\n";
    choose
      [
        choice (read_input ()) (fun str ->
            `ReadUser str);
        choice (Bqueue.recent_take handler_buf) (fun () ->
            `HandlerCalled)
      ] >>= fun pick ->
    match pick with
    | `ReadUser usr ->
      if not (String.contains usr ' ' || usr = "") then
        let okay_message = "Alrighty! We'll call you " ^ usr ^ ".\n" in
        printf_system "%s" okay_message;
        Keypersist.write_username usr keys |> return
      else
        (print_system "Usernames can not contain spaces, or be blank.\n";
         prompt_username keys)
    | `HandlerCalled ->
      print_system "\nBye!\n";
      Async.Std.exit(0)
  end
  else begin
    print_system ("Welcome back, ");
    print_username("@"^user);
    print_system(".\n");
    return keys
  end

let check_for_user_key keys =
  let null_key = {
    full_signing_key={n="0";e="0";d="0"};
    full_encryption_key={n="0";e="0";d="0"};
  } in
  if Keypersist.retrieve_user_key keys = null_key then
    begin
      print_system "Generating a fresh key pair.\n";
      let new_key = Crypto.gen_keys () in
      return (Keypersist.write_user_key new_key keys)
    end
  else
    return keys

let rec prompt_password () =
  print_system
    "Please enter your password. If this is your first time, type in your desired password.\n";
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
       check_for_user_key >>=
       prompt_username >>|
       process_keys_to_init
     with
       Persistence.Bad_password ->
       print_error "Incorrect password!\n";
       prompt_password ())
  | `HandlerCalled ->
    print_system "\nBye!\n";
    Async.Std.exit(0)

let _ =
  print_normal
    ("\n"^Logo.program_name^"\n\n\n");
  print_system "Welcome to SAJA (Siddant, Alex, Jacob, Amit) version 1.0.0.\n";
  print_system "Psst. You new around here? Type :help for help.\n";
  (Discovery.bind_discovery
     (fun online_user -> found := online_user::(!found)));
  let _ = listen chat_port handle_incoming_message in
  let _ = Discovery.start_listening () in
  let _ = prompt_password() >>| fun state -> main true state in
  let _ = Signal.handle [Signal.of_string "sigint"]
      ~f:(fun _ -> Bqueue.add () handler_buf) in
  Scheduler.go()
