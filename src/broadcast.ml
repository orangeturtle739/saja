open Core.Std
open Async.Std

(* [broadcast_address] is the address we send broadcasts to, and the address that
 * all online users listen to. *)
type broadcast_address = string

(* [send_broadcast broadcast_address] sends a
 * broadcast out to the network. It then
 * binds a callback to the event that packets are received
 * from the broadcast address which parses the packet
 * and adds the user_info obtained from the packet to a list.
 * After some time, the list is returned. *)

(* Citation: This implementation of UDP broadcasts is based on
  https://github.com/hverr/udptun/blob/7483c11c773c92bd4c6022329aebc33a843c64e6/bin/tunnel.ml
  *)

let send_broadcast : unit Deferred.t =
  let broadcast_address =
    (Socket.Address.Inet.create (Unix.Inet_addr.of_string "10.148.127.255") 3110) in
  let socket_fd = Unix.Socket.fd (Unix.Socket.(create Type.udp)) in
  let buffer = Iobuf.of_string "BROADCAST" in
  let send_func = Or_error.ok_exn (Udp.sendto()) in
  try_with ~extract_exn: true
    (fun () -> send_func socket_fd buffer broadcast_address) >>|
      function
      | Ok () -> ()
      | Error e -> raise e

(* [handle_broadcast] takes a function from online_user to a unit and
    returns a unit. It binds this function as a callback that processes
    the information received in a broadcast. *)
let handle_broadcast (callback : (Socket.Address.Inet.t -> string -> unit Deferred.t)) : unit Deferred.t =
  failwith "Unimplemented"

(* [respond] takes a function from unit to online_user
    and returns a unit. It binds this function as a callback
    that generates the response to a received broadcast. *)
let respond (broadcast_address : string) : int Deferred.t =
  failwith "Unimplemented"

let _ = after (Core.Std.sec 1.) >>=
  fun _ -> (send_broadcast >>| (fun _ -> print_endline "Broadcast sent."))

let _ = Scheduler.go()
