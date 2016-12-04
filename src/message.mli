type body = Msg of string | Init of (string * string) list | Join | Exit
type message

val to_string: message -> string

val from_string: string -> message option

val session_id: message -> string

val body: message -> body

val create: string -> body -> message
val msg: string -> string -> message
val init: string -> (string * string) list -> message
