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
  String.concat "\n"

let string_to_user str =
  let parts =
    try
      Str.split ( "\n" |> Str.regexp_string) str
    with
      Cryptokit.Error _ -> [] in
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
                 Msgtransport.send_msg ip_address tcp_port
  | None -> return true

let tcp_key_receive addr str = match string_to_user str with
  | Some user -> !discovery_callback {
      user = user;
      ip_address = addr;
    }
  | None -> ()

let start_listening () =
  Broadcast.bind_discovery (fun ip -> tcp_key_transmit ip |> ignore);
  Broadcast.start_listening ();
  let _ = Msgtransport.listen tcp_port tcp_key_receive in ()
