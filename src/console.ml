open Printf
open Async.Std
open Data

let stdin : Reader.t = Lazy.force Reader.stdin

let buf = Bqueue.create ()

let really_read_input () : string Deferred.t =
  Reader.read_line stdin >>= function
  | `Ok s -> return s
  | `Eof -> failwith "Uh-oh!"

(* Start read input buffering loop *)
let rec queue_input () = really_read_input () >>= fun s ->
  Bqueue.add s buf;
  queue_input ()
let _ = queue_input ()

let read_input () : string Deferred.t =
  Bqueue.take buf

let print_normal s =
  printf "%s" ("\x1b[0m"^s^"\x1b[0m")

let print_error s =
  printf "%s" ("\x1b[31m"^s^"\x1b[0m")

let print_system s =
  printf "%s" ("\x1b[33m"^s^"\x1b[0m")

let printf_system format = ksprintf print_system format
let printf_error format = ksprintf print_error format
let printf_message format = ksprintf print_error format
let printf_normal format = ksprintf print_normal format

let rec read_yes_no () : bool Deferred.t =

  let yes = function
    | "y" | "yes" -> true
    | _ -> false
  in

  let no = function
    | "n" | "no" -> true
    | _ -> false
  in

  read_input () >>= fun s ->
  let lower = String.lowercase_ascii s in
  if yes lower then return true
  else if no lower then return false
  else read_yes_no()
