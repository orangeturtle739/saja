open Yojson.Basic
open Data

type filename = string
exception Bad_password

(* [write_file file contents pass] writes JSON [contents] into [file] 
 * encrypted with [pass].*)
val write_file: filename -> json -> string -> unit

(* [read_file file pass] returns the JSON contents of a [file] decrypted
 * with [pass]. If [file] cannot be found, raises a [Sys_error].
 * If the password is bad, raiess a [Bad_password] *)
val read_file: filename -> string -> json
