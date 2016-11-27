open Core.Std
open Async.Std
open Data

(* This module contains functions to send and receive encrypted messages over
 * a network through Transmission Control Protocol (TCP). The TCP communicates
 * via a local network only. *)

(* [send_msg ip ms pt] sends [ms] which is an encrypted message to the client
 * located at IP version 4 address [ip] through port [pt]. Returns true if the
 * action succeded else returns false. *)
val send_msg: ip_address -> int -> encrypted_message -> bool Deferred.t

(* [handle_msg ()] takes a function from an encrypted string (message
    received) to a unit and returns a unit. This binds the function passed in
    as a callback whenever a message is received. *)
val listen: int -> (string -> unit) -> unit