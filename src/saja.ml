open Data
open Msgtransport
open Console
open Async.Std
open Keypersist

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
  username: username
}

let handle_discovery state =
  let found = ref [] in
  Discovery.bind_discovery
    (fun online_user -> found := online_user::(!found));
  let add_user {user={username;public_key}; ip_address} state =
    let state = {state with keys = write_key username public_key state.keys} in
    let state = {state with user_ips = (username, ip_address)::(List.remove_assoc username state.user_ips)} in
    return state in
  let rec process_users state = match !found with
    | [] -> return state
    | h::t -> found := t; add_user h state >>= process_users in
  process_users state

(* [execute] takes an action and a program state and returns
   a new program state with the action executed. *)
let execute (command: action) (state: program_state) : program_state Deferred.t =
  match command with
  | Discover -> handle_discovery state
  | StartSession user_lst -> failwith "???"
  | QuitProgram -> Async.Std.exit(0)
  | Help -> print_endline "Help is for the weak."; return state
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
  Console.read_input () >>= fun s ->
  execute (action_of_string s) program_state >>= fun new_state ->
  main new_state

let _ =
  print_endline "Welcome to";
  print_endline
    Logo.program_name;
  let _ = listen 12999 (fun addr str -> printf "Received: %s\nFound: %s" str addr) in
  let keys = Keypersist.load_keystore () in
  let keys = if Keypersist.retrieve_user_key keys = null_key then
      (print_endline "Generating a fresh key pair.";
       let new_key = Crypto.gen_keys () in Keypersist.write_user_key new_key keys)
    else keys in
  let keys = if Keypersist.retrieve_username keys = "" then
      (print_endline "What's your name, partner?";
       let new_user = "Billy" in
       print_endline ("Alrighty! We'll call you " ^ new_user ^ ".");
       Keypersist.write_username new_user keys)
    else keys in
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
  let _ = main {keys=keys; username="amit"; user_ips = []} in
  Scheduler.go()
