(** @author Kiran Mitra (km936), Rayhan Khanna (rk696), Shriya Sudhakar (ss3576) *)

open Position

type 'a t
(** ['a t] represents a square matrix with elements of type ['a]. *)

val get : position -> 'a t -> 'a
(** [get position matrix] gets the element at [position] in [matrix]. *)

val set : position -> 'a -> 'a t -> unit
(** [set position element matrix] puts [element] in [position] in [matrix]. *)

val initialize : 'a -> int -> 'a t
(** [initialize item size] gives a square matrix with side length [size] filled
    with [item] in all entries. *)

val clone : 'a t -> 'a t
(** [clone matrix] creates a copy of [matrix]. *)

val get_array : 'a t -> 'a array array
(** [get_array matrix] returns the underlying matrix as an array of arrays. This
    function is used for unit testing. *)

val iterate :
  ?position:position -> 'acc -> ('acc -> position -> 'a -> 'acc) -> 'a t -> 'acc
(** [iterate position acc fn] traverses the square matrix and performs [fn] on
    every element in the square matrix, returning [acc] after accumulating the
    result across the matrix. *)
