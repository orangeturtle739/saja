open Async.Std

type 'a t

(* BQueue is a blocking queue that allows for running concurrent asynchronous
   processes that mutate common state without interfering with each other. *)

(* [create] creates a Bqueue. *)
val create: unit -> 'a t

val take: 'a t -> 'a Deferred.t

val add: 'a -> 'a t -> unit

val forget: 'a t -> unit

val recent_take: 'a t -> 'a Deferred.t
