open Data
open Async.Std
open Async_unix

(* Documentation links:
 * https://ocaml.janestreet.com/ocaml-core/109.12.00/doc/core/Or_error.html
 * https://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html
 * https://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#VALsetsockopt
 * https://ocaml.janestreet.com/ocaml-core/111.28.00/doc/async_unix/#Std.Socket
 *)

let broadcast_string = "gem-broadcast"
let udp_port = 31100
let discovery_callback = ref (fun _ -> ())

(* [broadcast_address] gets the computer's broadcast addresses. *)
let broadcast_address : string = "255.255.255.255"

let send_broadcast (address : string) : bool Deferred.t =
  let broadcast_address =
    (Std.Socket.Address.Inet.create
       (Unix.Inet_addr.of_string address) ~port:udp_port) in
  let socket = Std.Socket.create Std.Socket.Type.udp in
  Std.Socket.setopt socket Std.Socket.Opt.broadcast true;
  let buffer = Core.Std.Iobuf.of_string broadcast_string in
  let send_func = Core.Std.Or_error.ok_exn (Udp.sendto ()) in
  Std.Socket.(bind socket (Address.Inet.create_bind_any ~port:0)) >>= fun sock ->
  (fun () -> send_func (Std.Socket.fd sock) buffer broadcast_address) |>
  try_with >>| (function
      | Core.Std.Ok () -> true
      | Core.Std.Error _ -> false)

let bind_discovery callback = discovery_callback := callback

let start_listening () =
  let socket = Std.Socket.create Std.Socket.Type.udp in
  Std.Socket.(bind socket
                (Address.Inet.create_bind_any ~port:udp_port)) >>= (fun sock ->
  Udp.recvfrom_loop (Std.Socket.fd sock) (fun message_buffer addr ->
      let address = Socket.Address.Inet.to_string addr in
      let message = Core.Std.Iobuf.to_string message_buffer in
      if message = broadcast_string then
        !discovery_callback address
      else ())) |> ignore

(* Demo function.*)
let test_udp () =
  bind_discovery (fun ip -> printf "Found peer at: %s\n" ip);
  start_listening () |> ignore ;
  Core.Std.sec 1. |> after >>= (fun _ ->
  send_broadcast broadcast_address >>| fun ok ->
  if ok then print_endline "OK!" else print_endline "bad") |> ignore;
  Scheduler.go()

(* Should be removed before submitting otherwise it will cause problems *)
let _ = test_udp ()
