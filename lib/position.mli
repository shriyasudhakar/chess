type position = {
  x : int;
  y : int;
}
(** [position] represents the position. *)

type direction = {
  len : int;
  step : position;
}
(** [direction] represents the number of steps something can move in a certain
    direction from a certain position. [step] represents the kind of step it can
    take (i.e. diagonally) and [len] represents the amount of such steps it can
    take from its current position. *)

val origin : position
(** [origin] represents the equivalent of the origin in the Cartesian coordinate
    plane. *)

val invalid : position
(** [invalid] represents an invalid position. *)

val make_position : int -> int -> position
(** [make_position] takes in x and y values and returns a position at those
    values. *)

val is_valid : position -> bool
(** [is_valid position] checks if [position] is valid. *)

val move : position -> position -> position
(** [move direction position] gives the new position when moving something from
    [position] in [direction]. *)

val move_by_row : ?len:int -> ?max:int -> position -> position
(** [move_by_row len max position] gives the position when the object moves
    [len] steps starting from [position] such that it is traversing the rows (of
    max width [max]). Requires: [max] > 0 and [position.y] and [position.x] are
    non-negative. *)

val direction : ?len:int -> int -> int -> direction
(** [direction len x y] gives a direction where the step is [x] in the x
    direction and [y] in the y direction and [len] is the amount of steps it can
    take. *)

val to_string : position -> string
(** [to_string position] gives the string representation of [postion]. *)
