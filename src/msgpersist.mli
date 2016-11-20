open Persistence
open Data

type chatlog = (username * message) list

(* [writelog file log] writes a chat history [log] to [file]. *)
val write_log: filename -> chatlog -> unit

(* [read_log file] returns the chatlog described by a [file]. *)
val read_log: filename -> chatlog
