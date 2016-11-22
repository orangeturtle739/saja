open Data
open Core.Std
open Async.Std
open Async_unix

let broadcast_string = "BROADCAST"
let udp_port = 31100
let exchange_port = 59999
let username = "amit" (* TODO replace with username specified in controller *)
let my_key = {n="test"; e="fake"; d="bogus"} (* TODO: use (retrieve_user_key (load_keystore())) in Controller *)

(* [get_broadcast_addresses] gets the computer's broadcast addresses. *)
let get_broadcast_address () : string =
  "192.168.1.255" (* TODO replace with scraped output from ifconfig *)

(* [serialize_user_info] serializes a username and a key into a string. *)
let serialize_user_info (username: string) (key : full_key) : string =
  username ^ " " ^ key.n ^ " " ^ key.e

(* The user info string looks like username^" "^n^" "^e. [deserialize_user_info] unpacks this into a user record.
    Returns unit for now. *)
let deserialize_user_info (msg: string) (addr: string) : unit =
  let _ =
    let tokens = String.split msg ~on:' ' in
    match tokens with
    | [u; n; e] -> let p = {n=n; e=e} in
      let user = {
        username=u;
        public_key=p}
      in
      {user=user; ip_address=addr}
    | _ -> failwith "Malformatted user info message."
  in ()

(* Citation: This implementation of UDP broadcasts is loosely based on
   https://github.com/hverr/udptun/blob/7483c11c773c92bd4c6022329aebc33a843c64e6/bin/tunnel.ml
   and
   https://github.com/tox4j/deprecated-tox4j/blob/a84a28e8c24821407652c2ed3c3bc43a1942a4ee/projects/tox4j/src/main/ocaml/core/network.ml
*)

(* [setup_exchange_server] sets up a TCP server that listens for messages from
    respondents to a broadcast. *)
let setup_exchange_server () : unit Deferred.t =
  let socket = print_endline "going to make socket"; Tcp.on_port exchange_port in
  let rec read_responses_callback = print_endline "made socket"; fun addr r w ->
      (* Callback when message received from client *)
      let buffer = String.create (128) in
      Reader.read r buffer >>= function
        (* [buffer] contains a respondent's public key. Need to store in the keystore if it's not there *)
      | `Eof -> print_endline "EOF????"; failwith "EOF"
      | `Ok msg -> print_endline "OK msg?"; (let addr_string = Socket.Address.Inet.to_string addr in
                                             deserialize_user_info addr_string buffer); return () (* TODO store user details in keypersist from Controller *)
        >>= fun () -> Writer.write w (serialize_user_info username my_key); Writer.flushed w >>= fun () -> read_responses_callback addr r w
  in
  let server = Tcp.Server.create socket
  in return(ignore(server))

(* [setup_exchange_client] establishes a TCP connection to an address [addr] that
    has sent a broadcast to the client. It then sends information over this
    TCP connection. *)
let setup_exchange_client (addr: string) : unit Deferred.t =
  let conn = Tcp.to_host_and_port addr exchange_port in
  Tcp.connect conn  >>= fun (addr,r,w) ->
  Writer.write w (serialize_user_info username my_key);
  Writer.flushed w >>= fun () ->
  (* Read in response *)
  let buffer = String.create (128) in
  Reader.read r buffer >>= function
    (* [buffer] contains the new user's public key. Need to store in the keystore if it's not there *)
  | `Eof -> failwith "EOF"
  | `Ok msg -> return (print_endline buffer)
    >>= fun () -> return ()

(* [send_broadcast] sends a broadcast and sets up a TCP server
    to listen for information sent from respondents to the broadcast. *)
let rec send_broadcast (address : string) : unit Deferred.t =
  let broadcast_address =
    (Socket.Address.Inet.create (Unix.Inet_addr.of_string address) udp_port) in print_endline "foo 1";
  let socket = Std.Socket.create Std.Socket.Type.udp in
  Std.Socket.setopt socket Std.Socket.Opt.broadcast true;
  let buffer = Iobuf.of_string broadcast_string in print_endline "foo 3";
  let send_func = Or_error.ok_exn (Udp.sendto ()) in print_endline "foo 4";
  Std.Socket.bind socket (Std.Socket.Address.Inet.create_bind_any 0) >>= fun sock ->
  (fun () -> send_func (Std.Socket.fd sock) buffer broadcast_address) |>
  try_with >>| (function
      | Ok () -> print_endline "Sent."
      | Error (Unix.Unix_error (err, _, _)) -> print_endline "Error";
        Core.Std.Unix.error_message err |> print_endline) >>= fun _ ->
  Async.Std.after (Core.Std.sec 1.) >>= fun _ ->
  send_broadcast address

(* [listen_for_broadcast] listens for UDP broadcasts. *)
let listen_for_broadcast () : unit Deferred.t =
  print_endline "Started listening.";
  let socket = Std.Socket.create Std.Socket.Type.udp in
  Std.Socket.bind socket (Std.Socket.Address.Inet.create_bind_any udp_port) >>= fun sock ->
  Udp.recvfrom_loop (Std.Socket.fd sock) (fun message_buffer addr ->
      print_endline "Got it.";
      let address = Socket.Address.Inet.to_string addr in
      let message = Iobuf.to_string message_buffer in
      if message = broadcast_string then
        upon (setup_exchange_client address) (fun _ -> ())
      else ())

let _ = send_broadcast (get_broadcast_address())
let _ = listen_for_broadcast

let _ = Scheduler.go()
