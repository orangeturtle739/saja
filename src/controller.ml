open Data
open Msgtransport
open Console
open Async.Std

(* [action] represents an action taken. *)
type action =
  | Discover
  | StartSession of (username list)
  | QuitProgram
  | Help
  | SendMessage of (session_id * message)
  | GetInfo of session_id
  | ExitSession of session_id

(* [program state] is a representation type containing the relevant
   details of the program's state. *)
type program_state = {
    keys: Keypersist.t;
    username: username
}

(* [execute] takes an action and a program state and returns
   a new program state with the action executed. *)
let execute (command: action) (state: program_state) : program_state Deferred.t =
    match command with
    | Discover -> failwith "AMIT"
    | StartSession user_lst -> failwith "???"
    | QuitProgram -> failwith "Unimplemented"
    | Help -> failwith "Unimplemented"
    | SendMessage msg -> failwith "Unimplemented"
    | GetInfo session -> failwith "Unimplemented"
    | ExitSession session -> failwith "Unimplemented"

let action_of_string (s : string) :

let rec main program_state =
  let _ = listen 12999 (fun addr str -> printf "Received: %s\nFound: %s" str addr) in
    Console.read_input () >>= fun s ->
      execute (action_of_string s) >>= fun new_state ->
        main new_state

let () =
    print_system
        Logo.program_name;
    let keys = Keypersist.load_keystore () in
    main keys
