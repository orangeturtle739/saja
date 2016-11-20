open Yojson.Basic
open Data

type filename = string

(* [write_file file contents] writes JSON [contents] into [file].*)
let write_file (file: filename) (j: json) =
	to_file file j

(* [read_file file] returns the JSON contents of a [file]. If [file] cannot
 * be found, raises a [Sys_error]. *)
let read_file (file: filename) =
	from_file file