open Data
open Async.Std

(* This module controls the I/O processes for the console. *)

(* [read_input ()] reads the string typed bu the user into the console
 * returns the string. *)
val read_input: unit -> message Deferred.t

(* [print_message message] prints a message to the console. *)
val print_message: message -> unit

val print_error: string -> unit

val print_system : string -> unit
