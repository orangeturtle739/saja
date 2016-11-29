open Persistence
open Data

(* The type of a key store. *)
type t

(* [load_keystore] returns the currently written rendition of the key store
 * from file. *)
val load_keystore: unit -> t

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

(* [retrieve_keys store] is an association list containing all known
 * verified username-public key pairs in [store]. *)
val retrieve_keys: t -> (username * public_key_pair) list

(* [retrieve_user_key store] is the full key of the user stored in the
 * key store. *)
val retrieve_user_key: t -> full_key_pair