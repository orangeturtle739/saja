open Yojson.Basic
open Data

exception Bad_password

(* [write_file file contents pass] writes JSON [contents] into [file] 
 * encrypted with [pass].*)
val write_file: filename -> json -> string -> unit

(* [read_file file pass] returns the JSON contents of a [file] decrypted
 * with [pass]. If [file] cannot be found, raises a [Sys_error].
 * If the password is bad, raiess a [Bad_password] *)
val read_file: filename -> string -> json

(* [unenc_write_file file contents] writes JSON [contents] into [file].*)
val unenc_write_file: filename -> json -> unit

(* [unenc_read_file file] returns the JSON contents of a [file]. If [file] cannot
 * be found, raises a [Sys_error]. *)
val unenc_read_file: filename -> json