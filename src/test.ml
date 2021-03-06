open OUnit2
open Crypto
open Data

let alice = Crypto.gen_keys ()
let bob = Crypto.gen_keys ()
let bob_public = {n = bob.full_encryption_key.n; e = bob.full_encryption_key.e}
let bob_private = {n = bob.full_encryption_key.n; d = bob.full_encryption_key.d}
let alice_private = {n = alice.full_signing_key.n; d = alice.full_signing_key.d}
let alice_public = {n = alice.full_signing_key.n; e = alice.full_signing_key.e}

let tests = "test suite" >::: [
    "crypt_loop"  >::
    (fun _ -> assert_equal (Some ("Hi bob!", alice_public)) (
         Crypto.encrypt bob_public alice_private "Hi bob!" |>
         Crypto.decrypt [alice_public] bob_private));
    "pass_crypt_loop"  >::
    (fun _ -> assert_equal (Some "Hi bob!")
        (Crypto.pass_encrypt "password" "Hi bob!" |>
         Crypto.pass_decrypt "password"));
  ]

let _ = run_test_tt_main tests
