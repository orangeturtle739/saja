open Cryptokit
open Data

let encrypt pub_e_key pri_s_key session_id message = ("2","foo")

let decrypt key_id_list private_key message = Some ("2", "foo")

(* [advance id] evaluates to the next ID in the sequence. Every time a message
 * within a session is received, the session ID should be advanced. *)
let advance session_id = 2

let gen_keys () = failwith "Unimplemented"
