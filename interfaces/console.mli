(* This module controls the I/O processes for the console. *)

(* [read_input ()] reads the string typed bu the user into the console
 * returns the string. *)
val read_input: unit -> message

(* [print_to_console s] prints the string [s] to the console. Returns
 * a unit. *)
val print_to_console: message -> unit