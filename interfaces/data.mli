(* The type of a public key, to be replaced with whatever cryptokit
 * uses *)
type public_key = int list
(* The type of a private key, to be replaced with whatever cryptokit
 * uses *)
type private_key = int list

(* Type of the session id. Will be a 128 bit number (2 62 bit numbers)*)
type session_id = int64 * int64

(* The type of a a username *)
type username = string
(* A user *)
type user = {
    username: username;
    public_key: public_key;
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

