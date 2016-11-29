open Async.Std
open Data

let send_msg ip port msg =
  let connector = fun () -> Tcp.connect (Tcp.to_host_and_port ip port) in
  connector |> try_with >>= (function
    | Core.Std.Ok (_,_,w)-> Writer.write w msg;
                            Writer.close w >>= fun () -> return true
    | Core.Std.Error   _ -> return false)

let listen port callback =
  let terminal = Tcp.on_port port in
  let _server  = Tcp.Server.create terminal
      (fun address r _ -> Reader.contents r >>= fun contents ->
        let str_addr = Socket.Address.Inet.addr address |>
                       Unix.Inet_addr.to_string in
        callback str_addr contents |> return) in ()

let tcp_demo () =
  listen 3654 (fun addr str -> printf "Received: %s\nFound: %s" str addr);
  let _ = after (Core.Std.sec 1.) >>= fun _ ->
    send_msg "localhost" 3654
      "556688 Crpytic Message from Jacob Glueck!" >>| (fun suc ->
        if suc then print_endline "Sent" else print_endline "Error sending") in
  Scheduler.go ()

(* Test code remove before submission *)
(* let _ = tcp_demo () *)