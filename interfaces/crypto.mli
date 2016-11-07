
(* Type of the session id. Will be a 128 bit number (2 62 bit numbers)*)
type session_id = int64 * int64
(* Type of a public key. Will be whatever cryptokit uses *)
type public_key = int list
(* Type of a private key. Will be whatever cryptokit uses *)
type private_key = int list

(*[encrypt recipient_public_key signing_key sessiond_id message] returns the
 * pair (next_session_id, encrypted_message). The returned encrypted message
 * is base 64 enocded. *)
val encrypt : public_key -> private_key -> session_id -> string ->
  session_id * string

(*[decrypt public_signing_keys decryption_key sessiond_ids message] returns the
 * optinal pair (session_id, decrypted_message). If none of the keys in
 * [public_signing_keys] can verify the signature, this function evaluates to
 * None. If this function is able to verify the signautre, but unable to match
 * the session id in the message with a session id in [sessiond_ids], this
 * method returns None.*)
val decrypt : public_key list -> private_key -> session_id list -> string
  -> (session_id * string) option

(*[advance id] evaluates to the next ID in the sequence. Every time a message
 * within a session is received, the session ID should be advanced. *)
val advance : session_id -> session_id
