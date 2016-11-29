open Data

let stdin : Async.Std.Reader.t = Lazy.force Async.Std.Reader.stdin

let read_input () : string Async.Std.Deferred.t =
  Async.Std.(
    Reader.read_line stdin >>= function
    | `Ok s -> return s
    | `Eof -> failwith "Uh-oh!")

let print_message (message : message) : unit =
  ANSITerminal.(print_string [white] message)

let print_error (error : string) : unit =
  ANSITerminal.(print_string [red] error)

let print_system (msg : string) : unit =
  ANSITerminal.(print_string [yellow] msg)
