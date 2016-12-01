open Msgtransport
open Async.Std

let tcp_demo () =
  let _ = listen 3654 (fun addr str -> printf "Received: %s\nFound: %s" str addr) in
  let _ = after (Core.Std.sec 1.) >>= fun _ ->
    send_msg "localhost" 3654
      "556688 Crpytic Message from Jacob Glueck!" >>| (fun suc ->
        if suc then print_endline "Sent" else print_endline "Error sending") in
  Scheduler.go ()

let _ = tcp_demo ()
