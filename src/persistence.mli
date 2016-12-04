open Yojson.Basic
open Data

(* [write_file file contents pass] writes JSON [contents] into [file] 
 * encrypted with [pass].*)
val write_file: filename -> json -> string -> unit

(* [read_file file pass] returns the JSON contents of a [file] decrypted
 * with [pass]. If [file] cannot be found, raises a [Sys_error].
 * If the password is bad, raises a [Bad_password]. If the JSON is
 * malformed, raises a [Malformed_file]. *)
val read_file: filename -> string -> json

(* [unenc_write_file file contents] writes JSON [contents] into [file].*)
val unenc_write_file: filename -> json -> unit

(* [unenc_read_file file] returns the JSON contents of a [file]. If [file]
 * cannot be found, raises a [Sys_error]. If the JSON is malformed, 
 * raises a [Malformed_file] *)
val unenc_read_file: filename -> json