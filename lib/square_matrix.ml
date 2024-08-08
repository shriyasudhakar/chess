(** Abstraction Function: The array [arr] represents a square matrix. The
    element in the square matrix at row x and column y is in [arr.(x).(y)].
    Representation Invariant: It is a square, so the number of rows is equal to
    the number of columns. *)

open Position

type 'a t = 'a array array

let get position matrix = matrix.(position.y).(position.x)
let set position element matrix = matrix.(position.y).(position.x) <- element
let initialize item size = Array.make_matrix size size item
let clone matrix = Array.(map copy) matrix
let get_array matrix = matrix

let rec iterate ?(position = origin) acc (fn : 'acc -> position -> 'a -> 'acc)
    matrix =
  let size = Array.length matrix in
  if position.y = size then acc
  else
    iterate
      ~position:(move_by_row position ~max:size)
      (fn acc position (get position matrix))
      fn matrix
