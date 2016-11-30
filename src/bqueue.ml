open Async.Std

type 'a t = 'a Ivar.t Queue.t * 'a Queue.t

let create () =
  (Queue.create (), Queue.create ())

let rec take (to_fill, ready) =
  if Queue.is_empty ready then (
    let next = Ivar.create () in
    Queue.add next to_fill;
    Ivar.read next
  ) else Queue.pop ready |> return

let add thing (to_fill, ready) =
  if Queue.is_empty to_fill then Queue.add thing ready
  else Ivar.fill (Queue.pop to_fill) thing

let forget (to_fill, ready) = Queue.clear to_fill

let recent_take thing =
  forget thing;
  take thing
