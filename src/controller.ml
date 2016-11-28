open Data
open Msgtransport
open Console

(* [action] represents an action taken. *)
type action =
  | Discover
  | StartSession of online_user list
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
let execute (command: action) (state: program_state) =
    match command with
    | Discover -> failwith "AMIT"
    | StartSession user_lst -> failwith "???"
    | QuitProgram -> failwith "Unimplemented"
    | Help -> failwith "Unimplemented"
    | SendMessage msg -> failwith "Unimplemented"
    | GetInfo session -> failwith "Unimplemented"
    | ExitSession session -> failwith "Unimplemented"

let main keys =
    let _ = listen 12999 print_endline in
    let _ = Console.read_input () in
    ()

let () =
    print_system
        "Welcome to GEM - Glueck Encrypted Messaging.\n";
    let keys = Keypersist.load_keystore () in
    main keys
