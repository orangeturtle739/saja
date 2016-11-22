open Data
open Core.Std
open Async.Std
open Async_unix

let broadcast_string = "gem-broadcast"
let udp_port = 31100
let discovery_callback = ref (fun _ -> ())

(* [broadcast_address] gets the computer's broadcast addresses. *)
let broadcast_address : string = "255.255.255.255"

let rec send_broadcast (address : string) : bool Deferred.t =
  print_endline ":((()))";
  let broadcast_address =
    (Socket.Address.Inet.create (Unix.Inet_addr.of_string address) udp_port) in
  let socket = Std.Socket.create Std.Socket.Type.udp in
  Std.Socket.setopt socket Std.Socket.Opt.broadcast true;
  let buffer = Iobuf.of_string broadcast_string in
  let send_func = Or_error.ok_exn (Udp.sendto ()) in
  Std.Socket.(bind socket (Address.Inet.create_bind_any 0)) >>= fun sock ->
  (fun () -> send_func (Std.Socket.fd sock) buffer broadcast_address) |>
  try_with >>| (function
      | Ok () -> true
      | Error (Unix.Unix_error (err, _, _)) -> false)

let bind_discovery callback = discovery_callback := callback

(* [listen_for_broadcast] listens for UDP broadcasts and invokes the
 * discovery_callback if needed *)
let listen_for_broadcast () : unit Deferred.t =
  print_endline "starting";
  let socket = Std.Socket.create Std.Socket.Type.udp in
  Std.Socket.(bind socket (Address.Inet.create_bind_any udp_port)) >>= fun sock ->
  print_endline "foo";
  Udp.recvfrom_loop (Std.Socket.fd sock) (fun message_buffer addr ->
      let address = Socket.Address.Inet.to_string addr in
      let message = Iobuf.to_string message_buffer in
      if message = broadcast_string then
        let _ = Format.printf "Found peer at sad : %s\n" address in
        (!discovery_callback address)
      else ())

(* Start listening for broadcasts *)
let _ = listen_for_broadcast ()

(* Demo code. Should be removed before submitting otherwise it will cause
 * problems *)
let _ = bind_discovery (fun ip -> Format.printf "Found peer at: %s\n" ip)
let _ = Core.Std.sec 1. |> after >>= fun _ ->
  send_broadcast broadcast_address >>| fun ok ->
  if ok then print_endline "OK!" else print_endline "bad"

let _ = Scheduler.go()
