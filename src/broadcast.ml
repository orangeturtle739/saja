open Core.Std
open Async.Std

let broadcast_string = "BROADCAST"
let udp_port = 3110
let exchange_port = 5999

let get_broadcast_address () : string =
  "10.148.127.255"

(* Citation: This implementation of UDP broadcasts is loosely based on
  https://github.com/hverr/udptun/blob/7483c11c773c92bd4c6022329aebc33a843c64e6/bin/tunnel.ml
  and
  https://github.com/tox4j/deprecated-tox4j/blob/a84a28e8c24821407652c2ed3c3bc43a1942a4ee/projects/tox4j/src/main/ocaml/core/network.ml
  *)

(* [setup_exchange_server] sets up a TCP server that listens for messages from
    respondents to a broadcast. *)
let setup_exchange_server : unit Deferred.t =
  let socket = Tcp.on_port exchange_port in
  let server = Tcp.Server.create socket
    (fun addr r w ->
      (* Callback when message received from client *)
      let buffer = String.create (128) in
      Reader.read r buffer >>= function
        | `Eof -> return (print_endline "A client sent an empty message.")
        | `Ok msg -> return (print_endline "TODO: Do something with client information here...")
      >>= fun () -> Writer.write w "TODO: Response goes here"; Writer.flushed w >>= fun () -> return ())
  in return(ignore(server))

(* [setup_exchange_client] establishes a TCP connection to an address [addr] that
    has sent a broadcast to the client. It then sends information over this
    TCP connection. *)
let setup_exchange_client (addr: string) : unit Deferred.t =
  let conn = Tcp.to_host_and_port addr exchange_port in
  Tcp.connect conn  >>= fun (addr,r,w) ->
    Writer.write w "Information goes here";
    Writer.flushed w >>= fun () ->
      (* Read in response *)
      let buffer = String.create (128) in
      Reader.read r buffer >>= function
        | `Eof -> return (print_endline "A client sent an empty message.")
        | `Ok msg -> return (print_endline "TODO: Do something with server information here...")
      >>= fun () -> return ()

(* [send_broadcast] sends a broadcast and sets up a TCP server
    to listen for information sent from respondents to the broadcast. *)
let send_broadcast : unit Deferred.t =
  let broadcast_address =
    (Socket.Address.Inet.create (Unix.Inet_addr.of_string (get_broadcast_address ()) ) udp_port) in
  let socket_fd = Unix.Socket.fd (Unix.Socket.(create Type.udp)) in
  let buffer = Iobuf.of_string broadcast_string in
  let send_func = Or_error.ok_exn (Udp.sendto ()) in
  try_with ~extract_exn:true
    (fun () -> send_func socket_fd buffer broadcast_address) >>=
      function
      | Ok () -> setup_exchange_server
      | Error (Unix.Unix_error (err, _, _)) -> return (print_endline
        (Core.Std.Unix.error_message err))
    >>= fun () -> return ()

(* [listen_for_broadcast] listens for UDP broadcasts. *)
let listen_for_broadcast : unit Deferred.t =
  let socket_fd = Unix.Socket.fd (Unix.Socket.(create Type.udp)) in
  Udp.recvfrom_loop socket_fd (fun message_buffer addr ->
    let address = Socket.Address.Inet.to_string addr in
    let message = Iobuf.to_string message_buffer in
    if message = broadcast_string then
      upon (setup_exchange_client address) (fun _ -> ())
    else ())

let _ = after (Core.Std.sec 1.) >>=
  fun _ -> listen_for_broadcast >>= fun _ -> send_broadcast >>| fun _ -> print_endline "Broadcast sent."

let _ = Scheduler.go()
