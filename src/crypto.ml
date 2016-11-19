open Cryptokit
open Data

let rand = Cryptokit.Random.device_rng "/dev/urandom";;
let default_key_size = 4096
let rsa_block_size = default_key_size / 8
let rsa_payload_size = rsa_block_size - 1
let hash_function () = Hash.sha512 ()
let hash_length = 64
let aes_key_length = 32
let aes_block_size = 16
let padding = Padding.length
let chaining = Cipher.CBC

let string_to_hex str =
  let to_hex i =
    if i <= 9 then int_of_char '0' + i |> char_of_int else
      int_of_char 'A' + i - 10 |> char_of_int in
  let rec do_work str i =
    if i = String.length str then "" else
      let c = String.get str i |> int_of_char in
      let a = String.make 1 (c / 16 |> to_hex) in
      let b = String.make 1 (c mod 16 |> to_hex) in
      a^b^(do_work str (i+1)) in
  do_work str 0

let to_public_key key =
  {
    n = key.RSA.n;
    e = key.RSA.e;
  }
let to_private_key key =
  {
    n = key.RSA.n;
    d = key.RSA.d;
  }
let to_full_key key =
  {
    n = key.RSA.n;
    e = key.RSA.e;
    d = key.RSA.d;
  }
let from_public_key (key:public_key) =
  {
    RSA.size = (Format.printf "Converted public key size: %i\n" ((String.length key.n) * 8); (String.length key.n) * 8);
    RSA.n = key.n;
    RSA.e = key.e;
    RSA.d = "";
    RSA.p = "";
    RSA.q = "";
    RSA.dp = "";
    RSA.dq = "";
    RSA.qinv = "";
  }
let from_private_key (key:private_key) =
  {
    RSA.size = (Format.printf "Converted private key size: %i\n" ((String.length key.n) * 8); (String.length key.n) * 8);
    RSA.n = key.n;
    RSA.d = key.d;
    RSA.e = "";
    RSA.p = "";
    RSA.q = "";
    RSA.dp = "";
    RSA.dq = "";
    RSA.qinv = "";
  }
let from_full_keykey (key:full_key) =
  {
    RSA.size = (Format.printf "Converted full key size: %i\n" ((String.length key.n) * 8); (String.length key.n) * 8);
    RSA.n = key.n;
    RSA.e = key.e;
    RSA.d = key.d;
    RSA.p = "";
    RSA.q = "";
    RSA.dp = "";
    RSA.dq = "";
    RSA.qinv = "";
  }

type aes_encryption_cipher = {
  iv: string;
  key: string;
  cipher: transform;
}
let zero_pad message =
  let zeros = String.make (rsa_block_size - (String.length message)) (char_of_int 0) in
  Format.printf "Adding 0: %i\nFinal length: %i\n" (String.length zeros) (String.length (zeros^message));
  Format.printf "Final zero padded message: %s\n" ((zeros^message) |> string_to_hex);
  zeros^message
let gen_aes_key () = let gen_key = Random.string rand rsa_payload_size in
  Format.printf "Generated AES key: %s\nKey length: %i\n" (gen_key |> string_to_hex) (String.length gen_key); gen_key
let extract_aes_key key = String.sub key 0 aes_key_length
let gen_iv () = Random.string rand aes_block_size
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
let aes_decryption iv key =
  Cipher.aes ~mode:chaining ~pad:padding ~iv:iv
    (extract_aes_key key) Cipher.Decrypt
let aes_encrypt encryption = transform_string (encryption.cipher)
let aes_decrypt decryption = transform_string decryption

let rsa_encrypt (public_key:public_key) message = RSA.encrypt (from_public_key public_key) message |> zero_pad
let hmac message = hash_string (hash_function ()) message
let padded_hmac message =
  let hash = hmac message in
  Format.printf "Generated HMAC: %s\nHMAC length: %i\n" (hash |> string_to_hex) (String.length hash);
  let tail =
    Random.string rand (rsa_payload_size - hash_length) in
  let result = hash^tail in
  Format.printf "Generated padded HMAC: %s\nPadded HMAC length: %i\n" (result |> string_to_hex) (String.length result);
  result
let rsa_sign private_key message =
  padded_hmac message |> (fun foo -> let nice_sig = ( foo |> string_to_hex) in Format.printf "Signed: %s\nSig length: %i\n" nice_sig (String.length foo); foo) |>RSA.sign (from_private_key private_key) |> zero_pad
let rsa_decrypt private_key message =
  let almost = RSA.decrypt (from_private_key private_key) message in
  String.sub almost 1 rsa_payload_size
let rsa_verify public_key message signature =
  Format.printf "Signature: %s\n Signature length: %i\n"  ( signature |> string_to_hex) (String.length signature);
  Format.printf "Body: %s\n Body length: %i\n"  ( message |> string_to_hex) (String.length message);
  let padded_signed_hmac =
    RSA.unwrap_signature (from_public_key public_key) signature in Format.printf "Unwrapped sig: %s\nSig length: %i\n" ( padded_signed_hmac |> string_to_hex) (String.length padded_signed_hmac);
  let signed_hmac = String.sub padded_signed_hmac 1 hash_length in
  let expected_hmac = hmac message in Format.printf "Unwrapped HMAC: %s\nLength: %i\n" ( signed_hmac |> string_to_hex) (String.length signed_hmac); Format.printf "Expected HMAC: %s\nLength: %i\n" ( expected_hmac |> string_to_hex) (String.length expected_hmac);
  if signed_hmac = expected_hmac then Some message else None


let encrypt (pub_e_key:public_key) (pri_s_key:private_key) (message:string) =
  let session_encryption = aes_encryption () in
  let encrypted_session_key = rsa_encrypt pub_e_key (session_encryption.key) in
  let encrypted_body = aes_encrypt session_encryption message in
  let payload = encrypted_session_key^(session_encryption.iv)^encrypted_body in
  let signature = rsa_sign pri_s_key payload in
  let result = payload^signature in
  transform_string (Base64.encode_multiline ()) result

let try_decrypt public_key private_key data =
  try
    let decoded = transform_string (Base64.decode ()) data in print_endline "1";
    let message =
      String.sub decoded 0 (String.length decoded - rsa_block_size) in  print_endline "2";
    let signature = String.sub decoded
        (String.length decoded - rsa_block_size) rsa_block_size in  print_endline "3";
    match rsa_verify public_key message signature with
    | None ->  print_endline "4"; String.length message |> print_int; print_newline (); String.length signature |> print_int; print_newline (); print_endline "DFSDFSDF"; None
    | Some message ->
      let encrypted_session_key = String.sub message 0 rsa_block_size in  print_endline "5";
      let iv =
        String.sub message rsa_block_size aes_block_size in  print_endline "6";
      let encrypted_body = String.sub message
          (rsa_block_size + aes_block_size) (String.length message - (rsa_block_size + aes_block_size)) in print_endline "7";
      let session_key = rsa_decrypt private_key encrypted_session_key in  print_endline "8";
      let session_decryption = aes_decryption iv session_key in  print_endline "9";
      Some (aes_decrypt session_decryption encrypted_body)
  with
  | Invalid_argument _ -> print_endline "Ahh"; None

let rec decrypt public_key_list private_key message = match public_key_list with
  | [] -> None
  | h::t -> let attempt = try_decrypt h private_key message in
    if attempt <> None then attempt else decrypt t private_key message

(* [advance id] evaluates to the next ID in the sequence. Every time a message
 * within a session is received, the session ID should be advanced. *)
let advance session_id = 2

let gen_keys () =
  let encryption_key = RSA.new_key ~rng:rand default_key_size in
  let signing_key = RSA.new_key ~rng:rand default_key_size in
  {
    full_encryption_key = to_full_key encryption_key;
    full_signing_key = to_full_key signing_key;
  }
