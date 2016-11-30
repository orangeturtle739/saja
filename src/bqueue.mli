open Async.Std

type 'a t

val create: unit -> 'a t
val take: 'a t -> 'a Deferred.t
val add: 'a -> 'a t -> unit
val forget: 'a t -> unit
val recent_take: 'a t -> 'a Deferred.t
