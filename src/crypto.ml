open Cryptokit
open Data

(* Use urandom: http://www.2uo.de/myths-about-urandom/ *)
let rand = Cryptokit.Random.device_rng "/dev/urandom"
(* 4096 bit RSA encryption *)
let default_key_size = 4096
(* Size in bytes of encrypted RSA block (512) *)
let rsa_block_size = default_key_size / 8
(* Size of payload in encrypted RSA block. The value being encrypted has to
 * be < n, so just use 1 byte smaller (recommended by cryptokit documentation) *)
let rsa_payload_size = rsa_block_size - 1
(* Use a SHA512 hash for all hashing *)
let hash_function () = Hash.sha512 ()
(* Length of the output of the hash function, in bytes *)
let hash_length = 64
(* Length of the AES key, in bytes. 32 bytes is 256 bits, the biggest supported
 * AES key *)
let aes_key_length = 32
(* The number of bytes in each AES block *)
let aes_block_size = 16
(* The type of padding used for AES (PKCS 5) *)
let padding = Padding.length
(* The block chaining mode (Cipher Block Chaining) *)
let chaining = Cipher.CBC

(* Converts a cryptokit RSA key to a Data.full_key *)
let to_full_key (key:RSA.key) =
  {
    n = key.RSA.n;
    e = key.RSA.e;
    d = key.RSA.d;
  }
(* Converts a Data.public_key to a cryptokit RSA key *)
let from_public_key (key:public_key) =
  {
    RSA.size = (String.length key.n) * 8;
    RSA.n = key.n;
    RSA.e = key.e;
    RSA.d = "";
    RSA.p = "";
    RSA.q = "";
    RSA.dp = "";
    RSA.dq = "";
    RSA.qinv = "";
  }
(* Converts a Data.private_key to a cryptokit RSA key *)
let from_private_key (key:private_key) =
  {
    RSA.size = (String.length key.n) * 8;
    RSA.n = key.n;
    RSA.d = key.d;
    RSA.e = "";
    RSA.p = "";
    RSA.q = "";
    RSA.dp = "";
    RSA.dq = "";
    RSA.qinv = "";
  }
(* represents: an AES encryption cipher, with a transform, an initialization
 * vector (iv), and a key. *)
type aes_encryption_cipher = {
  iv: string;
  key: string;
  cipher: transform;
}
(* Generates a random string of size [rsa_payload_size]. The first
 * [aes_key_length] bytes will be used as the AES session key. *)
let gen_aes_key () = Random.string rand rsa_payload_size
(* Extracts the first [aes_key_length] bytes from a string to use as the AES
 * session key *)
let extract_aes_key key = String.sub key 0 aes_key_length
(* Generates a random AES initialization vector for CBC *)
let gen_iv () = Random.string rand aes_block_size
(* Generates a random AES encryption setup *)
let aes_encryption () =
  let iv = gen_iv () in
  let key = gen_aes_key () in
  let cipher = Cipher.aes ~mode:chaining ~pad:padding ~iv:iv
      (extract_aes_key key) Cipher.Encrypt in
  {
    iv;
    key;
    cipher;
  }
(* Generates an AES decryption transform given the initialization vector and
 * key. *)
let aes_decryption iv key =
  Cipher.aes ~mode:chaining ~pad:padding ~iv:iv
    (extract_aes_key key) Cipher.Decrypt
(* [aes_encrypt encryption message] encrypts the given message using the
 * [encryption] setup. *)
let aes_encrypt encryption = transform_string (encryption.cipher)
(* [aes_decrypt decryption message] decrypts the given message using the
 * [decryption] setup. *)
let aes_decrypt decryption = transform_string decryption

(* [rsa_encrypt public_key message] encrypts the specified message with the
 * public key *)
let rsa_encrypt (public_key:public_key) message =
  RSA.encrypt (from_public_key public_key) message
(* Computes a Hashed Message Authentication Code (HMAC) for a given message
 * uses [hash_function ()]. *)
let hmac message = hash_string (hash_function ()) message
(* Computes a HMAC for a message and pads it with random data to the
 * [rsa_payload_size]. *)
let padded_hmac message =
  let hash = hmac message in
  let tail =
    Random.string rand (rsa_payload_size - hash_length) in
  hash^tail
(* Signs a message using a private key and a paddded HMAC *)
let rsa_sign private_key message =
  padded_hmac message |> RSA.sign (from_private_key private_key)
(* Decrypts a message using the private key *)
let rsa_decrypt private_key message =
  let almost = RSA.decrypt (from_private_key private_key) message in
  String.sub almost 1 rsa_payload_size
(* Verifies the signature for a message.
 * returns: [None] if the signature was invalid, and [Some message] if the
 * signature was valid. *)
let rsa_verify public_key message signature =
  let padded_signed_hmac =
    RSA.unwrap_signature (from_public_key public_key) signature in
  let signed_hmac = String.sub padded_signed_hmac 1 hash_length in
  let expected_hmac = hmac message in
  if signed_hmac = expected_hmac then Some message else None

(* Encrypts the given message with the public key and signs the encrypted
 * message with the private key *)
let encrypt (pub_e_key:public_key) (pri_s_key:private_key) (message:string) =
  let session_encryption = aes_encryption () in
  let encrypted_session_key = rsa_encrypt pub_e_key (session_encryption.key) in
  let encrypted_body = aes_encrypt session_encryption message in
  let payload = encrypted_session_key^(session_encryption.iv)^encrypted_body in
  let signature = rsa_sign pri_s_key payload in
  let result = payload^signature in
  transform_string (Base64.encode_multiline ()) result

(* Bind because OCaml does not have it like Haskell *)
let (>>=) x f = match x with
  | None -> None
  | Some x -> f x

(* Tries to decrypt a signed message and verify the signature. If the signature
 * is valid, decrypts the message using the private key.
 * returns: [None] if the signature was not valid, and
 * [Some (plaintext, public_key)] if the signature was valid. *)
let try_decrypt public_key private_key data =
  try
    let decoded = transform_string (Base64.decode ()) data in
    let message =
      String.sub decoded 0 (String.length decoded - rsa_block_size) in
    let signature = String.sub decoded
        (String.length decoded - rsa_block_size) rsa_block_size in
    rsa_verify public_key message signature >>= fun message ->
    let encrypted_session_key = String.sub message 0 rsa_block_size in
    let iv =
      String.sub message rsa_block_size aes_block_size in
    let encrypted_body = String.sub message
        (rsa_block_size + aes_block_size) (String.length message - (rsa_block_size + aes_block_size)) in
    let session_key = rsa_decrypt private_key encrypted_session_key in
    let session_decryption = aes_decryption iv session_key in
    Some (aes_decrypt session_decryption encrypted_body, public_key)
  with
  (* This happens if the message is too short and a substring fails. In that
   * case, it is not valid. *)
  | Invalid_argument _ -> None
  | Cryptokit.Error _ -> None

(* Tries to verify the signature of the message with each public key.
 * If there is a public key which produces a valid signature, returns
 * [Some (plaintext, public_key)]. *)
let rec decrypt public_key_list private_key message = match public_key_list with
  | [] -> None
  | h::t -> let attempt = try_decrypt h private_key message in
    if attempt <> None then attempt else decrypt t private_key message

(* Computes the next session ID in the sequence *)
let advance session_id = hmac session_id |>
                         transform_string (Base64.encode_compact ())

(* Generates the a random initial session ID *)
let gen_session_id () = Random.string rand hash_length |>
                        transform_string (Base64.encode_compact ())

(* Generates a full RSA key pair *)
let gen_keys () =
  let encryption_key = RSA.new_key ~rng:rand default_key_size in
  let signing_key = RSA.new_key ~rng:rand default_key_size in
  {
    full_encryption_key = to_full_key encryption_key;
    full_signing_key = to_full_key signing_key;
  }

(* Computes the public fingerprint *)
let fingerprint (p_key_pair:public_key_pair) =
  let join (p:public_key) = p.n^p.e in
  let full = (join p_key_pair.signing_key)^(join p_key_pair.encryption_key) in
  hmac full |> transform_string (Base64.encode_compact ())
(* Computes the public fingerprint *)
let fingerprint_f (f_key_pair:full_key_pair) =
  let extract_public_key (key:full_key) = {
    n = key.n;
    e = key.e;
  } in
  fingerprint {
    signing_key = extract_public_key f_key_pair.full_signing_key;
    encryption_key = extract_public_key f_key_pair.full_encryption_key;
  }

(* Encrypts text using a symetric cipher and with the scrypt key derivation
 * algorithm *)
let pass_encrypt password thing = Scrypt.encrypt_exn thing password |>
                                  transform_string (Base64.encode_compact ())
(* Decrypts text using a symetric cipher and with the scrypt key derivation
 * algorithm.
 * returns: [None] if the password was wrong, [Some plaintext] otherwise. *)
let pass_decrypt  password thing = Scrypt.decrypt
    (thing |> transform_string (Base64.decode ())) password
