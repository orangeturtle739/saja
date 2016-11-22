open Async.Std
open Data

(* ================================================================= *)

let relay_packets ibuf r w =
  Reader.read r ibuf
  >>= function
  | `Eof        -> return ()
  | `Ok str_len ->
    Writer.write w ibuf ~len:str_len;
    print_endline (String.sub ibuf 0 str_len); (* replace with callback *)
    Writer.flushed w

let send_msg ip port msg =
  try
    Tcp.connect (Tcp.to_host_and_port ip port) >>=
    fun (_,_,w) -> Writer.write w msg;
    Writer.flushed w >>= fun () -> return true
  with _ -> return false

let listen () =
  print_endline "Welcome";
  let terminal = Tcp.on_port 12999 in
  let _server  = Tcp.Server.create terminal
  (fun _ r w -> let ibuf = String.make 4096 '.' in
   relay_packets ibuf r w) in ()

(* send_msg "localhost" 12999 "556688 Crpytic Message from Jacob Glueck!"*)