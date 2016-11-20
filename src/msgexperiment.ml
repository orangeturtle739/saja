open Core.Std
open Async.Std

let print s =
  printf "%s\n" s

let send_msg ip port msg =
    let conn = Tcp.to_host_and_port ip port in
    Tcp.connect conn  >>= fun (addr,r,w) ->
    Reader.read r msg >>= function
    | `Eof -> return false
    | `Ok text ->
      Writer.write w msg;
      Writer.flushed w >>=
        fun () -> return true

let _ = send_msg "0.0.0.0" 5554 "This is the message"

let _ = Scheduler.go()