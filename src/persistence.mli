open Yojson.Basic
open Data

type filename = string

(* [write_file file contents] writes JSON [contents] into [file].*)
val write_file: filename -> json -> unit

(* [read_file file] returns the JSON contents of a [file]. *)
val read_file: filename -> json
