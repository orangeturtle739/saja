open Yojson.Basic
open Persistence
open Data

module Store = Map.Make(String)

(* The type of a key store. *)
type t = {outside_keys: public_key Store.t; user_key: private_key}

(* The name of the file where key data is stored. *)
let keyfile = "keys.json"

(* [key_to_json] parses a user-key pair into a JSON friendly tuple. *)
let key_to_json (user, (key: public_key)) =
	(user, `Assoc [("n", `String key.n); ("e", `String key.e)])

(* [key_from_json] parses a user-key pair from JSON to a tuple. *)
let key_from_json (user, (key: json)) =
	Util.((user, {n = key |> member "n" |> to_string;
				  e = key |> member "e" |> to_string}))

(* [save_keystore store] saves the key store [store] to file. *)
let save_keystore (store: t) =
	let j = `Assoc [
	(
		"public", 
		`Assoc (store.outside_keys |> Store.bindings |> List.map key_to_json)
	);
	(
		"private",
		(`Assoc [
			("n", `String store.user_key.n); 
			("d", `String store.user_key.d)
		])
	)
	]
	in write_file keyfile j

(* [load_keystore] returns the currently written rendition of the key store
 * from file. *)
let load_keystore () =
	try
		let j = read_file keyfile in
		let public_key_list = Util.(j |> member "public" |> to_assoc) |> 
					   		  List.map key_from_json in
		let public_keys = List.fold_left (fun s (user, key) ->
										 Store.add user key s) 
						  Store.empty public_key_list in
		let user_key = j |> Util.member "private" in
		let user_key_info = {n = Util.(user_key |> member "n" |> to_string);
							 d = Util.(user_key |> member "d" |> to_string)}
		in {outside_keys = public_keys; user_key = user_key_info}
	with
		Sys_error _ -> let empty_store = {
			outside_keys = Store.empty;
			user_key = {n = "0"; d = "0"}
		} in
		save_keystore empty_store;
		empty_store

(* [verify_key name key] is [true] if public key [key] is associated with
 * user [name]. *)
let verify_key (user: username) (key: public_key) (store: t) =
	try
		Store.find user store.outside_keys = key
	with
		Not_found -> false

(* [write_key name key store] is [store] with [key] associated with
 * [name]. If user [name] already had an associated key, it is replaced
 * with the new key. *)
let write_key (user: username) (key: public_key) (store: t) =
	let new_store = 
		{store with outside_keys = Store.add user key store.outside_keys} 
	in save_keystore new_store; new_store

(* [write_private_key key store] is [store] updated with the new private
 * [key]. *)
let write_private_key (key: private_key) (store: t) =
	let new_store = 
		{store with user_key = key} 
	in save_keystore new_store; new_store

(* [retrieve_keys store] is an association list containing all known
 * verified username-public key pairs in [store]. *)
let retrieve_keys (store: t) =
	Store.bindings store.outside_keys

(* [retrieve_private_key store] is the private key stored in the key store. *)
let retrieve_private_key (store: t) =
	store.user_key