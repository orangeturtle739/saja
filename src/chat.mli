open Data

type t

val join: session_id -> online_user list -> t
val check_join: session_id -> online_user list ->
  (session_id * (online_user list)) option
val create: online_user list -> t
val receive_msg: online_user -> session_id -> string -> t -> t option
val receive_join: online_user -> session_id -> t -> t option
val receive_exit: online_user -> session_id -> t -> t option
val send_msg: string -> string -> t ->
  Message.body * t * ((session_id * username * ip_address) list)
val send_init: string -> t ->
  Message.body * t * ((session_id * username * ip_address) list)
val send_join: string -> t ->
  Message.body * t * ((session_id * username * ip_address) list)
val send_exit: string -> t ->
  Message.body * t * ((session_id * username * ip_address) list)
val info: t -> (username * ip_address) list
val msg_log: t -> (username * message) list
