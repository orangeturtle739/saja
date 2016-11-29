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
  | SendMessage of message
  | GetInfo
  | ExitSession

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
    | Discover -> failwith "Foo"
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
    let _ = main {keys=keys; username="amit"} in
    Scheduler.go()
