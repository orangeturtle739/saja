open Async.Std
open Data

(* ================================================================= *)

let relay_packets ibuf r w callback=
  Reader.read r ibuf
  >>= function
  | `Eof        -> return ()
  | `Ok str_len ->
    Writer.write w ibuf ~len:str_len;
    callback (String.sub ibuf 0 str_len);
    Writer.flushed w

let send_msg ip port msg =
  try
    Tcp.connect (Tcp.to_host_and_port ip port) >>=
    fun (_,_,w) -> Writer.write w msg;
    Writer.flushed w >>= fun () -> return true
  with _ -> return false

let listen port callback =
  print_endline "Welcome";
  let terminal = Tcp.on_port port in
  let _server  = Tcp.Server.create terminal
  (fun _ r w -> let ibuf = String.make 5120 '.' in
   relay_packets ibuf r w callback) in ()

(* send_msg "localhost" 12999 "556688 Crpytic Message from Jacob Glueck!"*)