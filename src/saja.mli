open Data
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
  | TransmitKeys of ip_address
  | ProcessUsers
  | Fingerprint of username

(* [program state] is a representation type containing the relevant
   details of the program's state. *)
type program_state

(* [execute] takes an action and a program state and returns
   a new program state with the action executed. *)
val execute: action -> program_state -> program_state Deferred.t
