open Broadcast
open Async.Std

(* Demo function.*)
let test_udp () =
  bind_discovery (fun ip -> printf "Found peer at: %s\n" ip);
  start_listening () |> ignore ;
  Core.Std.sec 1. |> after >>= (fun _ ->
      send_broadcast () >>| fun ok ->
      if ok then print_endline "OK!" else print_endline "bad") |> ignore;
  Scheduler.go()

let _ = test_udp ()
