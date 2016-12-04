open Yojson.Basic
open Data
open Crypto

(* [write_file file contents pass] writes JSON [contents] into [file] 
 * encrypted with [pass].*)
let write_file (file: filename) (j: json) (password: string) =
  `String (j |> to_string |> pass_encrypt password) |> to_file file

(* [read_file file pass] returns the JSON contents of a [file] decrypted
 * with [pass]. If [file] cannot be found, raises a [Sys_error].
 * If the password is bad, raises a [Bad_password] *)
let read_file (file: filename) (password: string) =
  try
    let contents = from_file file |> to_string |>
                   Str.global_replace (Str.regexp "^\"\\|\"$") "" |>
                   pass_decrypt password in
    match contents with
    | Some stuff -> from_string stuff
    | None -> raise Bad_password
  with
    Yojson.Json_error _ -> raise Malformed_file

(* [unenc_write_file file contents] writes JSON [contents] into [file].*)
let unenc_write_file (file: filename) (j: json) =
  to_file file j

(* [unenc_read_file file] returns the JSON contents of a [file]. If [file] cannot
 * be found, raises a [Sys_error]. *)
let unenc_read_file (file: filename) =
  try
    from_file file
  with
    Yojson.Json_error _ -> raise Malformed_file