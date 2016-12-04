open Async.Std
open Data
open Console

(* Error message for to kill processes on port *)
let kill_port = "Kill processes in the port to continue.\n"

(* [trace p] returns the error message when port [p] is clogged. *)
let trace port = printf_error "\nAccess denied to port: %n. " port;
  print_error kill_port

(* [send_msg add p m] sends the string [m] to the IP address [add]
 * communicating on port [p] via TCP.
 * Returns: true Deferred.t if message was sent else false Deferred.t *)
let send_msg ip port msg =
  let connector = fun () -> Tcp.connect (Tcp.to_host_and_port ip port) in
  connector |> try_with >>= (function
      | Core.Std.Ok (_,_,w)-> Writer.write w msg;
        Writer.close w >>= fun () -> return true
      | Core.Std.Error   _ -> return false)

(* [listen p f] creates and initiates a TCP server which listens on port [p]
 * and applies function [f] on the string received everytime. *)
let listen port callback =
  let terminal = Tcp.on_port port in
  let server  = fun () -> Tcp.Server.create terminal
      (fun address r _ -> Reader.contents r >>= fun contents ->
        let str_addr = Socket.Address.Inet.addr address |>
                       Unix.Inet_addr.to_string in callback str_addr contents
                                                   |> return) in
  server |> try_with >>| (function
      | Core.Std.Ok _    -> ()
      | Core.Std.Error _ -> trace port;
        print_error "Saja is exiting.\n";
        ignore (Async.Std.exit(0)))