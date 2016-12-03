open Persistence
open Data

(* The type of a key store. *)
type t

(* [load_keystore pass] returns the currently written rendition 
 * of the key store from file decrypted with pass. *)
val load_keystore: string -> t

(* [save_keystore store] saves the key store [store] to file. *)
val save_keystore: t -> unit

(* [verify_key name key store] is [true] if public key [key] is associated 
 * with user [name]. *)
val verify_key: username -> public_key_pair -> t -> bool

(* [write_key name key store] is [store] with [key] associated with
 * [name]. If user [name] already had an associated key, it is replaced
 * with the new key. *)
val write_key: username -> public_key_pair -> t -> t

(* [write_username user store] updates the username in [store] to 
 * [user]. *)
val write_username: username -> t -> t

(* [write_user_key key store] is [store] updated with the new user
 * [key] and username [user]. *)
val write_user_key: full_key_pair -> t -> t

(* [user_stored user store] is [true] if [user] is stored in
 * the [store]. *)
val user_stored: username -> t -> bool

(* [retrieve_key user store] is an the public key pair of [user] in
 * [store]. *)
val retrieve_key: username -> t -> public_key_pair

(* [retrieve_fingerprint_user fp store] is Some [username] associated
 * with the fingerprint in [store], or None if no such user exists. *)
val retrieve_fingerprint_user: string -> t -> username option

(* [retrieve_user key store] is the username corresponding to public [key]
 * pair in [store]. *)
val retrieve_user: public_key_pair -> t -> username

(* [retrieve_keys store] is an association list containing all known
 * verified username-public key pairs in [store]. *)
val retrieve_keys: t -> (username * public_key_pair) list

(* [retrieve_username] is the username of the user stored in the
 * key store. *)
val retrieve_username: t -> username

(* [retrieve_user_key store] is the full key of the user stored in the
 * key store. *)
val retrieve_user_key: t -> full_key_pair