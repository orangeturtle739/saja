open Discovery
open Async.Std
open Data

let user_to_string user =
  [
    user.username;
    user.public_key.encryption_key.n;
    user.public_key.encryption_key.e;
    user.public_key.signing_key.n;
    user.public_key.signing_key.e;
  ] |>
  String.concat "\n"

(* Demo function.*)
let test_discovery () =
  let keys = Crypto.gen_keys () in
  let u = {
    username = "jacob";
    public_key = {
      encryption_key = {
        n = keys.full_encryption_key.n;
        e = keys.full_encryption_key.n;
      };
      signing_key = {
        n = keys.full_signing_key.n;
        e = keys.full_signing_key.e;
      }
    }
  } in
  set_key u;
  bind_discovery (fun user -> printf "Found peer: %s\nAt: %s\n" (user_to_string user.user) user.ip_address);
  start_listening ();
  print_endline "Started listening";
  Core.Std.sec 1. |> after >>= (fun _ ->
      send_broadcast () >>| fun ok ->
      if ok then print_endline "OK!" else print_endline "bad") |> ignore;
  Core.Std.sec 2. |> after >>= (fun _ ->
      send_broadcast () >>| fun ok ->
      if ok then print_endline "OK!" else print_endline "bad") |> ignore;
  Core.Std.sec 3. |> after >>= (fun _ ->
      send_broadcast () >>| fun ok ->
      if ok then print_endline "OK!" else print_endline "bad") |> ignore;
  Scheduler.go()

(* Should be removed before submitting otherwise it will cause problems *)
let _ = test_discovery ()
