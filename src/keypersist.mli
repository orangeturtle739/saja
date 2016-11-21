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
val verify_key: username -> public_key -> t -> bool

(* [write_key name key store] is [store] with [key] associated with
 * [name]. If user [name] already had an associated key, it is replaced
 * with the new key. *)
val write_key: username -> public_key -> t -> t

(* [write_private_key key store] is [store] updated with the new private
 * [key]. *)
val write_private_key: private_key -> t -> t

(* [retrieve_keys store] is an association list containing all known
 * verified username-public key pairs in [store]. *)
val retrieve_keys: t -> (username * public_key) list

(* [retrieve_private_key store] is the private key stored in the key store. *)
val retrieve_private_key: t -> private_key