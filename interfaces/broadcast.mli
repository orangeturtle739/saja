open Udp (* From Jane Street *)

(* A UDP broadcast sends a message to a "broadcast address" that all
  users listen to. *)

type broadcast_address = string
type user_info = {
  ip_address: string;
  public_key: string;
  username: string
}

(* Configures UDP settings. *)

val set_config: Config -> unit

(* Sends a broadcast out to the network.
  Binds a callback to the event that packets are received
  from the broadcast address which parses the packet
  and adds the user_info obtained from the packet to a list.
  After some time, the list is returned. *)

val send_broadcast: broadcast_address -> user_info list
