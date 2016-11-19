open Data

(* [encrypt recipient_public_key signing_key sessiond_id message] returns the
 * pair (next_session_id, encrypted_message). The returned encrypted message
 * is base 64 encoded. *)
val encrypt : public_key -> private_key -> string -> string

(* [decrypt public_signing_keys decryption_key sessiond_ids message] returns the
 * optional pair (session_id, decrypted_message). If none of the keys in
 * [public_signing_keys] can verify the signature, this function evaluates to
 * None. If this function is able to verify the signature, but unable to match
 * the session id in the message with a session id in [sessiond_ids], this
 * method returns None.*)
val decrypt : public_key list -> private_key -> string -> (string * public_key) option

val gen_keys : unit -> full_key_pair

(* [advance id] evaluates to the next ID in the sequence. Every time a message
 * within a session is received, the session ID should be advanced. *)
val advance : session_id -> session_id

(* Generates a new session ID *)
val gen_session_id : unit -> session_id

(* [pass_encrypt password thing] encrypts the thing with the specified
 * password. Returns None if there is a problem. *)
val pass_encrypt : string -> string -> string
(* [pass_decrypt password thing] decrypts the thing with the specified
 * password. Returns None if there is a problem (like a wrong password). *)
val pass_decrypt : string -> string -> string option
