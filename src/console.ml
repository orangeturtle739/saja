open Printf
open Async.Std
open Data

let white = "\x1b[0m"
let red = "\x1b[31m"
let yellow = "\x1b[33m"
let blue = "\x1b[34m"
let cyan = "\x1b[36m"
let green = "\x1b[32m"

let stdin : Reader.t = Lazy.force Reader.stdin

let buf = Bqueue.create ()

let really_read_input () : string Deferred.t =
  Reader.read_line stdin >>= function
  | `Ok s -> return s
  | `Eof -> return ""

(* Start read input buffering loop *)
let rec queue_input () = really_read_input () >>= fun s ->
  Bqueue.add s buf;
  queue_input ()
let _ = queue_input ()

let read_input () : string Deferred.t =
  Bqueue.recent_take buf

let print_normal s =
  printf "%s" (white^s^white)

let print_message s =
  printf "%s" (cyan^s^white)

let print_error s =
  printf "%s" (red^s^white)

let print_system s =
  printf "%s" (yellow^s^white)

let print_username s =
  printf "%s" (green^s^white)

let printf_system format = ksprintf print_system format
let printf_error format = ksprintf print_error format
let printf_message format = ksprintf print_message format
let printf_normal format = ksprintf print_normal format
let printf_username format = ksprintf print_username format

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

let printf_prompt prompt = 
  eprintf prompt
