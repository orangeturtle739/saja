open Data

(* [encrypt recipient_public_key signing_key sessiond_id message] returns the
 * pair (next_session_id, encrypted_message). The returned encrypted message
 * is base 64 encoded. *)
val encrypt : public_key -> private_key -> session_id -> string ->
  session_id * string

(* [decrypt public_signing_keys decryption_key sessiond_ids message] returns the
 * optional pair (session_id, decrypted_message). If none of the keys in
 * [public_signing_keys] can verify the signature, this function evaluates to
 * None. If this function is able to verify the signature, but unable to match
 * the session id in the message with a session id in [sessiond_ids], this
 * method returns None.*)
val decrypt : (public_key * session_id) list -> private_key -> string
  -> (session_id * string) option

val gen_keys : unit -> full_key_pair
