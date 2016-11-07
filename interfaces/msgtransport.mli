open Unix

(* This module contains functions to send and receive encrypted messages over
   a network through Transmission Control Protocol (TCP). The TCP communicates
   via a local network only. *)

type ipv4 = string

(* [send_msg ip m] sends [m] which is an encrypted message to the client
   located at IP version 4 (an ipv4) address [ip]. Returns true if the
   action succeded else returns false.*)
val send_msg: ipv4 -> string -> bool

(* [collect_msg ()] returns a list containing tuples of IP address and the
   respective encrypted message sent by the client. Returns [] if no
   messages have been recieved. *)
val collect_msg: unit -> (ipv4*string) list

(* [listen prt] listens to the any incoming messages from port [prt].
   Returns a unit. *)
val listen: int -> unit