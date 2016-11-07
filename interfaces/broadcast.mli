open Udp (* From Jane Street *)

module Broadcast = struct

  (* A UDP broadcast sends a message to a "broadcast address" that all
    users listen to. *)

  type broadcast_address = string

  (* Configures UDP settings. *)

  val set_config: Config -> ()

  (* Sends a broadcast out to the network and binds a callback to the event
    that packets are received from the broadcast address. *)

  val send_broadcast: broadcast_address -> ()

end
