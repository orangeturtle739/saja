# SAJA: Encrypted Peer-to-Peer Messaging}
_Saja_ (adj.) - "Calm, serene." [Arabic]
## Authors
* **S**iddant Basnet (sb846)
* **A**lex Libman (asl237)
* **J**acob Glueck (jng55)
* **A**mit Mizrahi (am2269)}

## Vision
SAJA is a peer-to-peer encrypted messaging system designed for communication between people on the same network.

### Key Features
* UDP based discovery of other connected users on the same network
* End-to-end encrypted messaging
* Group messaging
* Setting a username
* Exporting a chat log to a file

## Use
A user starts the program and has the ability to send out a UDP broadcast to discover other online users on the same network using the `:discover` command. This lists the IP addresses, user names, and public keys of these connected users. Alternatively, the user can manually send keys by using the `:transmit` command and process received keys from others using the `:process` command.

The user can choose to message any number of users that they know the details of, creating a group messaging session. The other users in this newly formed group receive a notification, inviting them to join the conversation. Once they do so, they will be able to send messages to everyone else who has agreed to join the group. If a user logs off and then logs back into a session, the client will request all of its peers for a history of what has happened during the logged-off period.

All messages sent using this system are encrypted and signed using a hybrid cryptosystem of RSA and AES. Every user maintains a keychain mapping usernames to public RSA keys. Every time a user starts a chat, the program verifies that the public key and username pair advertised with the chat matches that stored in the user's local keychain (this is a very simplified version of the PGP trust model). Group messages are represented as a grouping of pairwise messaging connections. Upon receiving a message, all clients verify the signature.

## Sample Session
```
You have been invited to a chat with:
  * @moby (192.168.1.232)
  * @rick (192.168.1.15)
  * @morty (192.168.1.103)
Would you like to join the chat? [y/n]
Joined chat.
@rick joined
@morty joined
@jim: Hello, folks!
@moby: We are all here!
@rick: Hello guys what's up!
@morty: Hey!
@moby: It works!
@jim: I sure am glad to be chatting with you.
@rick: Why I had such a great day today!
@jim: Great to hear.
@moby: Now we are safe from snoopers.
@jim: Woot!
@morty: Does anyone know how to clean orange juice off of dress pants?
@moby left the chat
@rick left the chat
@jim left the chat
```

## Demo
[![asciicast](https://asciinema.org/a/e9isbc0pb8ww5i36jjpck3dz6.png)](https://asciinema.org/a/e9isbc0pb8ww5i36jjpck3dz6)

# Dependencies

### OPAM packages
These are also found in `packages.txt`. Running `make install` in the top-level directory will install all of them.
* `async`
* `async-extra`
* `cryptokit`
* `ctypes`
* `ctypes-foreign`
* `scrypt`

### External dependencies
* `tmux`
* `libssl-dev`
* `libffi-dev`

## Protocol

### **This program was written by students for a course project. While we hope the cryptosystem is secure, you should not trust it. It is just for fun.**

### Discovery

The program can send a UDP broadcast to other peers on the network with the `:discover` command. It will then prompt the user to validate the key fingerprints of the received keys. If the user validates the fingerprints, it will store them permanently in the key store for future use.

### Initiation
When a user wishes to start a chat, they will enter the usernames of the people they wish to chat with. The initiator will then send a message to each of the other members of the group containing a unique session ID (different for each member) and a list of all other members in the chat.

### Message Transport
Message transport will be done pairwise, with each client sending a message individually to each other member of the group. Each chat has a session ID associated with it. The session id is set at initialization, and upon receiving the initialization message, each client hashes the session ID to use as the session ID for the chat.

When a client responds to the message, they will include this hashed session ID. This prevents the client from signing an arbitrary session ID. Without this hashing procedure, Eve could send a malicious initialization message to Alice containing a session ID containing something Alice would not want to sign, but she would sign anyways and send back. By hashing the session ID, it becomes computationally infeasible for Eve to generate a session ID such that when hashed would be malicious.

### Encryption
We used a hybrid-encryption scheme using 4096-bit RSA and 256-bit AES. RSA will be used to encrypt a session key for each message, and the body of the message will be encrypted using AES with that session key. The AES will use cipher block chaining.

### Key Verification
In order for the cryptosystem to be secure, each client must verify the keys of each user they communicate with over some secure channel, preferably in person. The program provides a `:fingerprint` command which prints out the public key fingerprint of the current user. The fingerprint is generated using a SHA-512 hash of the public encryption and signing keys.

A user can transmit their key to any other user by broadcasting it over UDP using `:discover` or directly with `:transmit <username | IP>`. Upon receiving a key, a user can add it to their keychain with `:process` (`:discover` will automatically invoke `:process` on any keys it receives; `:process` is only necessary when `:transmit` is used).

If there is no entry for the username in the keychain, the program will prompt the user to verify the fingerprint. If the key is already in the keychain and matches the one received, then the program will inform the user that it discovered a peer. If the user discovered is already stored in the keychain, but the key stored in the keychain does not match the key received, the program prints out a warning advising the user to reject the key because they user is probably the target of a man in the middle attack.

### Integrity

Each encrypted message is hashed with SHA-512 and then signed using a separate signing RSA key.

### Attack Analysis
To asses the security of the system, we considered the following attacks:
* Consider a man in the middle attack. Alice and Bob are trying to send messages to each other, but Eve pretends to be Bob, sending Bob's public keys to Alice from her own IP address. However, because Alice has verified Bob's key fingerprint in person, no matter where Alice sends her encrypted messages, a MITM attack will not allow Eve to decrypt the message because Alice won't encrypt the message with any key not equal to the verified key she has stored in her keychain.
* Consider a signing or encryption oracle attack. However, since each message only consists of the message the user wrote and a session ID, an attacker cannot force a client to encrypt and sign arbitrary data. With respect to the session ID, since each client hashes it on receiving it and uses the hashed version for all future communication, it is infeasible for an attack to engineer a session ID such that when hashed is something useful.

## Building

1. Install dependencies if needed.
1. Run `make` in the top-level directory.

## Running
To launch it, run `make run`. Alternatively, run `src/saja.sh` to launch the tmux version or run `src/saja.byte` to run it without tmux. `make debug` will also run the non-tmux version.

## Compatibility Issues

Due to issues with the `Asyc.Std.Udp` library, the broadcast function does not work with Mac OS X, so the `:discovery` command will not work on a Mac.
