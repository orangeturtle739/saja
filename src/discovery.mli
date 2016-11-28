open Data
open Async.Std

(* [send_broadcast broadcast_address] sends a
 * broadcast out to the network.
 * returns: true if the broadcast was successfull, false otherwise *)
val send_broadcast: unit -> bool Deferred.t

(* [set_key user] sets the user info to be sent out to the specified user info*)
val set_key: user -> unit

(* [bind_discovery func] takes a function from user to unit to be called
 * whenever a peer is discovered. *)
val bind_discovery: (user -> unit) -> unit

(* Makes the discovery module start listening and responding to incoming
 * broadcasts *)
val start_listening: unit -> unit
