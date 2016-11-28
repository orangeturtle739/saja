open Data
open Async.Std

let tcp_port = 89242
let current_user = ref None
let discovery_callback = ref (fun _ -> ())

let send_broadcast = Broadcast.send_broadcast

let set_key user = current_user := Some user

let bind_discovery func = discovery_callback := func

let user_to_string user =
  [
    user.username;
    user.public_key.encryption_key.n;
    user.public_key.encryption_key.e;
    user.public_key.signing_key.n;
    user.public_key.signing_key.e;
  ] |>
  List.map (Cryptokit.transform_string (Cryptokit.Base64.encode_compact ())) |>
  String.concat "\n"

let string_to_user str =
  let parts =
    Str.split ( "\n" |> Str.regexp_string) str |>
    List.map (fun str -> try Cryptokit.transform_string (Cryptokit.Base64.decode ()) str with _ -> print_endline "yelp"; "FOOBAR") in
  match parts with
  | name::en::ee::sn::se::[] -> Some {
      username = name;
      public_key = {
        encryption_key = {
          n = en;
          e = ee;
        };
        signing_key = {
          n = sn;
          e = se;
        }
      }
    }
  | _ -> None

let tcp_key_transmit ip_address = match !current_user with
  | Some user -> user_to_string user |>
                 Msgtransport.send_msg ip_address tcp_port |> ignore
  | None -> ()

let tcp_key_receive str = print_endline str; match string_to_user str with
  | Some user -> !discovery_callback user
  | None -> ()

let start_listening () =
  Broadcast.bind_discovery tcp_key_transmit;
  Broadcast.start_listening ();
  Msgtransport.listen tcp_port tcp_key_receive

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
  bind_discovery (fun user -> printf "Found peer: %s\n" (user_to_string user));
  start_listening ();
  Core.Std.sec 1. |> after >>= (fun _ ->
      send_broadcast () >>| fun ok ->
      if ok then print_endline "OK!" else print_endline "bad") |> ignore;
  Scheduler.go()

(* Should be removed before submitting otherwise it will cause problems *)
let _ = test_discovery ()
