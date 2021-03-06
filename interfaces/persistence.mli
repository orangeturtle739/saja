open Yojson.Basic
open Data

type filename = string

exception FileNotFound

(* [write_file file contents] writes JSON [contents] into [file].*)
val write_file: filename -> json -> unit

(* [read_file file] returns the JSON contents of a [file]. If [file] cannot
 * be found, raises [FileNotFound]. *)
val read_file: filename -> json
