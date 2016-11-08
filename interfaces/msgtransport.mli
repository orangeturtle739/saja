open Unix
open Data

(* This module contains functions to send and receive encrypted messages over
   a network through Transmission Control Protocol (TCP). The TCP communicates
   via a local network only. *)

(* [send_msg ip ms pt] sends [ms] which is an encrypted message to the client
   located at IP version 4 address [ip] through port [pt]. Returns true if the
   action succeded else returns false.*)
val send_msg: ipaddress -> encrypted_string -> int -> bool

(* [collect_msg ()] returns a list containing encrypted message sent by
   other clients. Returns [] if no messages have been recieved. *)
val collect_msg: unit -> encrypted_string list

(* [listen prt] listens to the any incoming messages from port [prt].
   Returns a unit. *)
val listen: int -> unit