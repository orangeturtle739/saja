open Data
open Async.Std

(* This module controls the I/O processes for the console. *)

(* [read_input ()] reads the string typed bu the user into the console
 * returns the string. *)
val read_input: unit -> message Deferred.t

(* [print_to_console message] prints a message to the console. *)
val print_to_console: message -> unit
