open Data

type t

(* [join session_id users] joins a chat with a given session ID and list of users. *)
val join: session_id -> online_user list -> t


(* [check_join] verifies a session ID and joins a session. *)
val check_join: session_id -> online_user list ->
  (session_id * (online_user list)) option

(* [create] creates a session based on an online user list. *)
val create: online_user list -> t

(* [receive_msg] updates the state based on a received message. *)
val receive_msg: online_user -> session_id -> string -> t -> t option

(* [receive_join] updates the state based on a received "join" message. *)
val receive_join: online_user -> session_id -> t -> t option

(* [receive_exit] updates the state based on a received "exit" message. *)
val receive_exit: online_user -> session_id -> t -> t option

(* [send_msg] *)
val send_msg: string -> string -> t ->
  Message.body * t * ((session_id * username * ip_address) list)

(* [send_init] *)
val send_init: string -> t ->
  Message.body * t * ((session_id * username * ip_address) list)

(* [send_join] *)
val send_join: string -> t ->
  Message.body * t * ((session_id * username * ip_address) list)

(* [send_exit] *)
val send_exit: string -> t ->
  Message.body * t * ((session_id * username * ip_address) list)

(* [info] *)
val info: t -> (username * ip_address) list

(* [msg_log] *)
val msg_log: t -> (username * message) list
