open Data
open Async.Std

(* [send_broadcast broadcast_address] sends a
 * broadcast out to the network. It then
 * binds a callback to the event that packets are received
 * from the broadcast address which parses the packet
 * and adds the user_info obtained from the packet to a list.
 * After some time, the list is returned. *)
val send_broadcast: ip_address -> unit Deferred.t

(* [handle_broadcast] takes a function from online_user to a unit and
    returns a unit. It binds this function as a callback that processes
    the information received in a broadcast. *)
(* val handle_broadcast: (online_user -> unit) -> unit *)

(* [respond] takes a function from unit to online_user
    and returns a unit. It binds this function as a callback
    that generates the response to a received broadcast. *)
(* val respond: (unit -> online_user) -> unit *)
