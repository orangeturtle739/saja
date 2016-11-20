open Yojson.Basic
open Persistence
open Data

module Store = Map.Make(String)

(* The type of a key store. *)
type t = public_key Store.t

(* The name of the file where key data is stored. *)
let keyfile = "keys.json"

(* [key_to_json] parses a user-key pair into a JSON friendly tuple. *)
let key_to_json (user, (key: public_key)) =
	(user, `Assoc [("n", `String key.n); ("e", `String key.e)])

(* [key_from_json] parses a user-key pair from JSON to a tuple. *)
let key_from_json (user, (key: json)) =
	Util.((user, {n = key |> member "n" |> to_string;
				  e = key |> member "e" |> to_string}))

(* [load_keystore] returns the currently written rendition of the key store
 * from file. *)
let load_keystore () =
	try
		let j = read_file keyfile in
		let key_list = Util.to_assoc j |> List.map key_from_json in
		List.fold_left (fun s (user, key) -> Store.add user key s) 
						Store.empty key_list
	with
		Sys_error _ -> failwith "No key file"

(* [save_keystore store] saves the key store [store] to file. *)
let save_keystore (store: t) =
	let j = `Assoc (Store.bindings store |> List.map key_to_json)
	in write_file keyfile j

(* [verify_key name key] is [true] if public key [key] is associated with
 * user [name]. *)
let verify_key (user: username) (key: public_key) store =
	try
		Store.find user store = key
	with
		Not_found -> false

(* [write_key name key store] is [store] with [key] associated with
 * [name]. If user [name] already had an associated key, it is replaced
 * with the new key. *)
let write_key (user: username) (key: public_key) store =
	let new_store = Store.add user key store in
	save_keystore new_store; new_store

(* [retrieve_keys store] is an association list containing all known
 * verified username-key pairs in [store]. *)
let retrieve_keys store =
	Store.bindings store