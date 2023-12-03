open Base

module Heap (Elt : Comparable.S) : sig
  type t =
    | Lf
    | Br of int * Elt.t * t * t

  val empty : t
  val is_empty : t -> bool
  val insert : Elt.t -> t -> t
  val merge : t -> t -> t
  val find_min : t -> Elt.t
  val delete_min : t -> t
end = struct
  type t =
    | Lf
    | Br of int * Elt.t * t * t

  let empty : t = Lf
  let is_empty : t -> bool = function Lf -> true | _ -> false
  let insert : Elt.t -> t -> t = failwith ""
  let merge : t -> t -> t =
    fun t1 t2 ->
    match t1, t2 with
    | Lf, _ -> t2
    |  _, Lf -> t1
    | Br (r1, a1, x1, y1), Br (r2, a2, x1, y1) ->
      failwith ""

  let find_min : t -> Elt.t = failwith ""
  let delete_min : t -> t = failwith ""
end
