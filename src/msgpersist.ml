open Yojson.Basic
open Persistence
open Data

type chatlog = (username * message) list

(* [writelog file log] writes a chat history [log] to [file]. *)
let write_log (file: filename) (log: chatlog) =
	let j = `List (List.map (fun (user, msg) -> 
							 `List [`String user; `String msg]) log) in
	write_file file j

(* [parse_msg msg] converts a JSON list representing a message entry
 * into a tuple (user, message). *)
let parse_msg (msg: json) =
	match Util.to_list msg with
	| [user; entry] -> Util.((to_string user, to_string entry))
	| _ -> failwith "Improperly stored chat message"

(* [read_log file] returns the chatlog described by a [file]. *)
let read_log (file: filename): chatlog =
	let j = from_file file in
	Util.to_list j |> List.map parse_msg