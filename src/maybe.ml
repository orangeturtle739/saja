(* Bind because OCaml does not have it like Haskell *)
let (>>>=) x f = match x with
  | Some thing -> f thing
  | None -> None
let (>>>|) x f = x >>>= fun thing -> Some (f thing)
let rec map_m f = function
  | [] -> Some []
  | h::t -> f h >>>= fun good_head ->
    map_m f t >>>| fun good_tail ->
    good_head::good_tail
