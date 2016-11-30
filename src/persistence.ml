open Yojson.Basic
open Data
open Crypto

type filename = string

(* [write_file file contents pass] writes JSON [contents] into [file] 
 * encrypted with [pass].*)
let write_file (file: filename) (j: json) (password: string) =
  `String (j |> to_string |> pass_encrypt password) |> to_file file

(* [read_file file pass] returns the JSON contents of a [file] decrypted
 * with [pass]. If [file] cannot be found, raises a [Sys_error]. *)
let read_file (file: filename) (password: string) =
  let contents = from_file file |> to_string |>
                 Str.global_replace (Str.regexp "^\"\\|\"$") "" |>
                 pass_decrypt password in
  match contents with
  | Some stuff -> from_string stuff
  | None -> failwith "Incorrect password"
