open Core.Std
open Async.Std
open Data

(* ================================================================= *)

let print s =
  printf "%s\n" s;
  return ()

let rec copy_blocks buffer r w =
  Reader.read r buffer
  >>= function
  | `Eof -> print_endline "Goodbye!"; return ()
  | `Ok bytes_read ->
    print_endline "Received!";
    Writer.write w buffer ~len:bytes_read;
    print_endline buffer;
    print_endline "Wrote.";
    Writer.flushed w

let send_msg ip port msg =
  print_endline "Sending...";
  let conn = Tcp.to_host_and_port ip port in
  Tcp.connect conn  >>= fun (addr,r,w) ->
  print_endline "Connection established.";
  Writer.write w msg;
  print_endline "Sent message!";
  Writer.flushed w >>=
  fun () -> return true

let listen () =
  print_endline "Server running";
  let socket = Tcp.on_port 12999 in
  print_endline "Port access gained";
  let server = Tcp.Server.create socket
  (fun addr r w -> print_endline "Processing!";
    let buffer = String.create (128) in
  copy_blocks buffer r w)
  in
  ignore (server : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

(*let _ = listen ()

let _ = after (Core.Std.sec 3.) >>=
  fun _ -> send_msg "localhost" 12999 "556688 Crpytic Message from Jacob Glueck!"

let _ = Scheduler.go()*)

let handle_msg f = ()