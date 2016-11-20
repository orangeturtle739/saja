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

let broadcast_string = "BROADCAST"
let port = 3110

let get_broadcast_address () : string =
  "10.148.127.255"

(* Citation: This implementation of UDP broadcasts is loosely based on
  https://github.com/hverr/udptun/blob/7483c11c773c92bd4c6022329aebc33a843c64e6/bin/tunnel.ml
  and
  https://github.com/tox4j/deprecated-tox4j/blob/a84a28e8c24821407652c2ed3c3bc43a1942a4ee/projects/tox4j/src/main/ocaml/core/network.ml
  *)

let send_broadcast : unit Deferred.t =
  let broadcast_address =
    (Socket.Address.Inet.create (Unix.Inet_addr.of_string (get_broadcast_address ()) ) port) in
  let socket_fd = Unix.Socket.fd (Unix.Socket.(create Type.udp)) in
  let buffer = Iobuf.of_string broadcast_string in
  let send_func = Or_error.ok_exn (Udp.sendto ()) in
  try_with ~extract_exn:true
    (fun () -> send_func socket_fd buffer broadcast_address) >>|
      function
      | Ok () -> ()
      | Error (Unix.Unix_error (err, _, _)) -> print_endline
        (Core.Std.Unix.error_message err)

(* [listen] listens for UDP broadcasts. *)
let listen : unit Deferred.t =
  let socket_fd = Unix.Socket.fd (Unix.Socket.(create Type.udp)) in
  Udp.recvfrom_loop socket_fd (fun message_buffer addr ->
    let address = Socket.Address.Inet.to_string addr in
    let message = Iobuf.to_string message_buffer in
    if message = broadcast_string then
      print_endline ("Received broadcast from " ^ address)
    else ())

(* [respond] takes a function from unit to online_user
    and returns a unit. It binds this function as a callback
    that generates the response to a received broadcast. *)
let respond (broadcast_address : string) : int Deferred.t =
  failwith "Unimplemented"

let _ = after (Core.Std.sec 1.) >>=
  fun _ -> (send_broadcast >>| (fun _ -> print_endline "Broadcast sent."))

let _ = Scheduler.go()
