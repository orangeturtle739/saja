open Core.Std
open Async.Std
open Data

(* ================================================================= *)

let relay_packets buffer r w =
  Reader.read r buffer
  >>= function
  | `Eof        -> return ()
  | `Ok str_len ->
    Writer.write w buffer ~len:str_len;
    print_endline (String.sub buffer 0 str_len);
    Writer.flushed w

let send_msg ip port msg =
  Tcp.connect (Tcp.to_host_and_port ip port) >>=
  fun (_,_,w) -> Writer.write w msg;
  Writer.flushed w >>= fun () -> return true

let listen () =
  print_endline "Welcome";
  let terminal = Tcp.on_port 12999 in
  let _server  = Tcp.Server.create terminal
  (fun _ r w -> let buffer = String.create 4096 in
   relay_packets buffer r w) in ()

let _ = listen ()

(* send_msg "localhost" 12999 "556688 Crpytic Message from Jacob Glueck!"*)

let _ = Scheduler.go()