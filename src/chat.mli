open Data

type t

val join: session_id -> online_user list -> t
val create: online_user list -> t
val receive_msg: online_user -> session_id -> string -> t -> t option
val send_msg: string -> string -> t -> t * (session_id * ip_address list)
