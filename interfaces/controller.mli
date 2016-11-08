open Data

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
type program_state

(* [execute] takes an action and a program state and returns
  a new program state with the action executed. *)
val execute:  action -> program_state -> program_state
