open Async.Std
open Data

let stdin : Reader.t = Lazy.force Reader.stdin

let read_input () : string Deferred.t =
  Reader.read_line stdin >>= function
  | `Ok s -> return s
  | `Eof -> failwith "Uh-oh!"

let print_message : ('a, unit, string, unit) format4 -> 'a =
  printf

let print_error : ('a, unit, string, unit) format4 -> 'a =
  printf "\x1b[0;31"; printf

let print_system : ('a, unit, string, unit) format4 -> 'a =
  printf "\x1b[1;33"; printf

let rec read_yes_no () : bool Deferred.t =

  let yes = function
    | "y" | "yes" -> true
    | _ -> false
  in

  let no = function
    | "n" | "no" -> true
    | _ -> false
  in

  Reader.read_line stdin >>= function
    | `Ok s -> let lower = String.lowercase_ascii s in
        if yes lower then return true
        else if no lower then return false
        else read_yes_no()
    | `Eof -> failwith "Uh-oh!"
