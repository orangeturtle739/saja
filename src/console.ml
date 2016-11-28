open Data
open Async.Std

let stdin : Reader.t = Lazy.force Reader.stdin

let read_input () : string Deferred.t =
  Reader.read_line stdin >>= function
    | `Ok s -> return s
    | `Eof -> failwith "Uh-oh!"

let print_message (message : message) : unit =
  ANSITerminal.(print_string [white] message)

let print_error (error : string) : unit =
  ANSITerminal.(print_string [red] error)

let print_system (msg : string) : unit =
  ANSITerminal.(print_string [yellow] msg)
