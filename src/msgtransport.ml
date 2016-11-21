open Core.Std
open Async.Std
open Data

(* ================================================================= *)

let rec copy_blocks buffer r w =
  Reader.read r buffer
  >>= function
  | `Eof -> print_endline "Goodbye!"; return ()
  | `Ok len ->
    print_endline "Received!";
    Writer.write w buffer ~len:len;
    let msg = String.sub buffer 0 len in
    print_endline msg;
    print_endline "Wrote.";
    Writer.flushed w

let send_msg ip port msg =
  print_endline "Sending...";
  let conn = Tcp.to_host_and_port ip port in
  Tcp.connect conn  >>= fun (addr,r,w) ->
  (*print_endline "Connection established.";*)
  Writer.write w msg;
  (*print_endline "Sent message!";*)
  Writer.flushed w >>=
  fun () -> return true

let listen () =
  print_endline "Server running";
  let socket = Tcp.on_port 12999 in
  print_endline "Port access gained";
  let server = Tcp.Server.create socket
  (fun addr r w ->
    let buffer = String.create (128) in
  copy_blocks buffer r w)
  in
  ignore (print_endline "Cheerio.")

let _ = listen ()

(* send_msg "localhost" 12999 "556688 Crpytic Message from Jacob Glueck!"*)

let _ = Scheduler.go()