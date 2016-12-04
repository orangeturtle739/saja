(* The type of a public RSA key *)
type public_key = {
  n: string;
  e: string;
}

(* The type of a private RSA key *)
type private_key = {
  n: string;
  d: string;
}

(* A full RSA key *)
type full_key = {
  n: string;
  e: string;
  d: string;
}

(* A full key pair, with a full signing key and a full encryption key *)
type full_key_pair = {
  full_signing_key: full_key;
  full_encryption_key: full_key;
}

(* A public key pair, with a public signing key for verifying signatures
 * and a public encryption key for encrypting messages *)
type public_key_pair = {
  signing_key: public_key;
  encryption_key: public_key;
}

(* Type of the session id.*)
type session_id = string

(* The type of a a username *)
type username = string
(* A user *)
type user = {
  username: username;
  public_key: public_key_pair;
}

(* They type of an IP address, to be changed to whatever the
 * Jane street networking module uses *)
type ip_address = string

(* Represents: an online user, which is a user with an IP
 * address *)
type online_user = {
  user: user;
  ip_address: ip_address;
}

(* The type of the messages sent over the network *)
type encrypted_message = string
(* The type of a plaintext message *)
type message = string

(* The type of a filename. *)
type filename = string