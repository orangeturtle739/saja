open Data
open Async.Std

(* [send_broadcast broadcast_address] sends a
 * broadcast out to the network.
 * returns: true if the broadcast was successfull, false otherwise *)
val send_broadcast: ip_address -> bool Deferred.t

(* [bind_discovery func] takes a function from ip_address to unit to be called
 * whenever a peer is discovered. The function takes the peer's IP address. *)
val bind_discovery: (ip_address -> unit) -> unit

(* Makes the broadcast module start listening and responding to incoming
 * broadcasts *)
val start_listening: unit -> unit
