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

(* [print_error] prints an error in red. *)
val print_error :  string -> unit

(* [printf_error] is a printf-like function for [print_error]. *)
val printf_error : ('a, unit, string, unit) format4 -> 'a

(* [print_system] prints a system message in yellow. *)
val print_system :  string -> unit

(* [printf_system] is a printf-like function for [print_system]. *)
val printf_system : ('a, unit, string, unit) format4 -> 'a

(* [print_message] prints a message in cyan. *)
val print_message: string -> unit

(* [printf_message] is a printf-like function for [print_message]. *)
val printf_message : ('a, unit, string, unit) format4 -> 'a

(* [print_username] prints a username. *)
val print_username: string -> unit

(* [printf_username] is a printf-like function for [print_username]. *)
val printf_username: ('a, unit, string, unit) format4 -> 'a

(* [print_normal] prints a username. *)
val print_normal: string -> unit

(* [printf_normal] is a printf-like function for [print_username]. *)
val printf_normal: ('a, unit, string, unit) format4 -> 'a


(* [print_prompt] prints the prompt ('>>='). *)
val print_prompt: string -> unit

(* [printf_prompt] is a printf-like function for [print_prompt]. *)
val printf_prompt: ('a, unit, string, unit) format4 -> 'a
