open Yojson.Basic
open Persistence
open Data

module Store = Map.Make(String)

(* The type of a key store. *)
type t = {
    outside_keys: public_key_pair Store.t;
    user: username;
    user_key: full_key_pair
}

(* The name of the file where key data is stored. *)
let keyfile = "keys.json"

(* [key_to_json] parses a user-key pair into a JSON friendly tuple. *)
let key_to_json (user, (key: public_key_pair)) =
    (user, 
    `Assoc [("sign", 
                `Assoc [("n", `String key.signing_key.n); 
                        ("e", `String key.signing_key.e)]);
            ("encrypt", 
                `Assoc [("n", `String key.encryption_key.n); 
                        ("e", `String key.encryption_key.e)])])

(* [key_from_json] parses a user-key pair from JSON to a tuple. *)
let key_from_json (user, (key: json)) =
    Util.((user, {
        signing_key = {
            n = key |> member "sign" |> member "n" |> to_string;
            e = key |> member "sign" |> member "e" |> to_string
        };
        encryption_key = {
            n = key |> member "sign" |> member "n" |> to_string;
            e = key |> member "sign" |> member "e" |> to_string
        }
    }))

(* [save_keystore store] saves the key store [store] to file. *)
let save_keystore (store: t) =
    let j = `Assoc [
    (
        "public", 
        `Assoc (store.outside_keys |> Store.bindings |> List.map key_to_json)
    );
    ("user", `String store.user);
    (
        "user_key",
        `Assoc [
            ("sign", `Assoc [
                ("n", `String store.user_key.full_signing_key.n);
                ("e", `String store.user_key.full_signing_key.e); 
                ("d", `String store.user_key.full_signing_key.d)
            ]);
            ("encrypt", `Assoc [
                ("n", `String store.user_key.full_encryption_key.n);
                ("e", `String store.user_key.full_encryption_key.e); 
                ("d", `String store.user_key.full_encryption_key.d)
            ])
        ]
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
        let user = j |> Util.member "user" |> to_string in
        let user_key = j |> Util.member "user_key" in
        let user_key_info = Util.({
            full_signing_key = {
                n = user_key |> member "sign" |> member "n" |> to_string;
                e = user_key |> member "sign" |> member "e" |> to_string;
                d = user_key |> member "sign" |> member "d" |> to_string
            };
            full_encryption_key = {
                n = user_key |> member "encrypt" |> member "n" |> to_string;
                e = user_key |> member "encrypt" |> member "e" |> to_string;
                d = user_key |> member "encrypt" |> member "d" |> to_string
            }
        })
        in {
            outside_keys = public_keys;
            user = user; 
            user_key = user_key_info
        }
    with
        Sys_error _ -> let empty_store = {
            outside_keys = Store.empty;
            user = "";
            user_key = {
                full_signing_key = {n = "0"; e = "0"; d = "0"};
                full_encryption_key = {n = "0"; e = "0"; d = "0"}
            }
        } in
        save_keystore empty_store;
        empty_store

(* [verify_key name key] is [true] if public key [key] is associated with
 * user [name]. *)
let verify_key (user: username) (key: public_key_pair) (store: t) =
    try
        Store.find user store.outside_keys = key
    with
        Not_found -> false

(* [write_key name key store] is [store] with [key] associated with
 * [name]. If user [name] already had an associated key, it is replaced
 * with the new key. *)
let write_key (user: username) (key: public_key_pair) (store: t) =
    let new_store = 
        {store with outside_keys = Store.add user key store.outside_keys} 
    in save_keystore new_store; new_store

(* [write_username user store] updates the username in [store] to 
 * [user]. *)
let write_username (user: username) (store: t) =
    let new_store = 
        {store with user = user} 
    in save_keystore new_store; new_store

(* [write_user_key key store] is [store] updated with the new user
 * [key]. *)
let write_user_key (key: full_key_pair) (store: t) =
    let new_store = 
        {store with user_key = key} 
    in save_keystore new_store; new_store

(* [user_stored user store] is [true] if [user] is stored in
 * the [store]. *)
let user_stored (user: username) (store: t) =
    Store.mem user store.outside_keys

(* [retrieve_key user store] is an the public key pair of [user] in
 * [store]. *)
let retrieve_key (user: username) (store: t) =
    try
        Store.find user store.outside_keys
    with
        Not_found -> failwith "No key stored for user"

(* [retrieve_keys store] is an association list containing all known
 * verified username-public key pairs in [store]. *)
let retrieve_keys (store: t) =
    Store.bindings store.outside_keys

(* [retrieve_username store] is the username of the user stored in the
 * key store. *)
let retrieve_username (store: t) =
    store.user

(* [retrieve_user_key store] is the full key of the user stored in the
 * key store. *)
let retrieve_user_key (store: t) =
    store.user_key