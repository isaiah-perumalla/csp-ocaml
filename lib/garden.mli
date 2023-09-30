
type pos = (int*int)

type t
val distance: pos -> pos -> int

(** width, vertical length, and return a garden.t **)
val create: int -> int ->  t

val width: t -> int
