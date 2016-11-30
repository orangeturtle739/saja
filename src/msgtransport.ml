open Async.Std
open Data
open Console

let trace port = "\n\nAccess denied to port: " ^ string_of_int(port) ^
  ". Kill processes in the port to continue. \n\nSaja is exiting.\n"

let send_msg ip port msg =
  let connector = fun () -> Tcp.connect (Tcp.to_host_and_port ip port) in
  connector |> try_with >>= (function
      | Core.Std.Ok (_,_,w)-> Writer.write w msg;
        Writer.close w >>= fun () -> return true
      | Core.Std.Error   _ -> return false)

let listen port callback =
  let terminal = Tcp.on_port port in
  let server  = fun () -> Tcp.Server.create terminal
      (fun address r _ -> Reader.contents r >>= fun contents ->
        let str_addr = Socket.Address.Inet.addr address |>
                       Unix.Inet_addr.to_string in
        callback str_addr contents |> return) in
  server |> try_with >>| (function
    | Core.Std.Ok a    -> ()
    | Core.Std.Error _ -> let tmsg = trace port in
                          print_error tmsg;
                          ignore (Async.Std.exit(0)))

let tcp_demo () =
  let a = listen 3654 (fun addr str -> printf "Received: %s\nFound: %s" str addr) in
  let _ = after (Core.Std.sec 1.) >>= fun _ ->
    send_msg "localhost" 3654
      "556688 Crpytic Message from Jacob Glueck!" >>| (fun suc ->
        if suc then print_endline "Sent" else print_endline "Error sending") in
  Scheduler.go ()

(* Test code remove before submission *)
(* let _ = tcp_demo () *)