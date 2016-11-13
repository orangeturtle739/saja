open Persistence
open Data

type chatlog = (username * string) list

(* [writelog file log] writes a chat history [log] to [file]. *)
val write_log: string -> chatlog -> unit

(* [read_log] file returns the chatlog described by a [file]. *)
val read_log: string -> chatlog