open Persistence

module type KeyStore = sig

	(* The type of a key store. *)
	type t

	(* [get_keystore] returns the currently written rendition of the key store
	 * from file. *)
	val get_keystore: unit -> t

	(* [save_keystore store] saves the key store [store] to file. *)
	val save_keystore: t -> unit

	(* [verify_key name key] is [true] if public key [key] is associated with
	 * user [name]. *)
	val verify_key: username -> public_key -> bool

	(* [write_key name key store] is [store] with [key] associated with
	 * [name]. If user [name] already had an associated key, it is replaced
	 * with the new key. *)
	val write_key: username -> public_key -> t -> t

	(* [retrieve_keys store] is an association list containing all known
	 * verified username-key pairs in [store]. *)
	val retrieve_keys: t -> (username * public_key) list
end
