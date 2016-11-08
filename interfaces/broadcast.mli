open Udp (* From Jane Street *)

(* [broadcast_address] is the address we send broadcasts to, and the address that
 * all online users listen to. *)
type broadcast_address = string

(* [set_config config] configures UDP settings. *)
val set_config: Config -> unit

(* [send_broadcast broadcast_address] sends a
 * broadcast out to the network. It then
 * binds a callback to the event that packets are received
 * from the broadcast address which parses the packet
 * and adds the user_info obtained from the packet to a list.
 * After some time, the list is returned. *)

val send_broadcast: broadcast_address -> online_user list
