open Async.Std

type 'a t = 'a Ivar.t ref * 'a Deferred.t Queue.t

let create () =
  let buf_tail = Ivar.create () |> ref in
  let buf =
    let queue = Queue.create () in
    Queue.add (Ivar.read !buf_tail) queue; queue in
  (buf_tail, buf)

let take (_, queue) =
  Queue.peek queue >>| fun first ->
  Queue.pop queue |> ignore;
  first

let add thing (tail, queue) =
  Ivar.fill !tail thing;
  tail := Ivar.create ();
  Queue.add (Ivar.read !tail) queue
