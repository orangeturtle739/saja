open Data
open Async.Std

(* This module controls the I/O processes for the console. *)

(* [read_input ()] reads the string typed bu the user into the console
 * returns the string. *)
val read_input: unit -> message Deferred.t

(* Effect: reads a one of [y, yes] and returns true, or
 * [n, no] and returns false. Keeps asking the user for input until the input
 * is one of the above values. *)
val read_yes_no: unit -> bool Deferred.t

(* [print_message message] prints a message to the console. *)
val print_message: ('a, unit, string, unit) format4 -> 'a

val print_error: ('a, unit, string, unit) format4 -> 'a

val print_system : ('a, unit, string, unit) format4 -> 'a 
