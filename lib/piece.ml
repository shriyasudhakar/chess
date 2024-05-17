type kind =
  | Empty
  | King
  | Queen
  | Rook
  | Knight
  | Bishop
  | Pawn

type player =
  | None
  | White
  | Black

type piece = {
  kind : kind;
  player : player;
  image : Graphics.image;
}

let _ = Display.initialize ()

let piece_string = function
  | Empty -> "Empty"
  | King -> "King"
  | Queen -> "Queen"
  | Rook -> "Rook"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Pawn -> "Pawn"

let player_string = function
  | None -> "None"
  | White -> "White"
  | Black -> "Black"

let create player kind =
  let image_filename player piece =
    Printf.sprintf "images/%s/%s.bmp" (player_string player)
      (piece_string piece)
  in
  let image = Bitmap_file.load (image_filename player kind) in
  { kind; player; image }

let pieces =
  [
    (None, Empty, create None Empty);
    (White, King, create White King);
    (White, Queen, create White Queen);
    (White, Rook, create White Rook);
    (White, Knight, create White Knight);
    (White, Bishop, create White Bishop);
    (White, Pawn, create White Pawn);
    (Black, King, create Black King);
    (Black, Queen, create Black Queen);
    (Black, Rook, create Black Rook);
    (Black, Knight, create Black Knight);
    (Black, Bishop, create Black Bishop);
    (Black, Pawn, create Black Pawn);
  ]

let lookup player piece =
  let rec loop_piece player piece = function
    | (player_type, piece_type, piece_instance) :: _
      when player_type = player && piece_type = piece -> piece_instance
    | _ :: t -> loop_piece player piece t
    | _ -> failwith "Piece not found"
  in
  loop_piece player piece pieces

let other = function
  | Black -> White
  | White -> Black
  | _ -> None

let empty = lookup None Empty

(** implement directions to show which places the pieces can move and the logic
    in game.valid_moves checks whether the move is valid in terms of jumping
    over pieces and whatnot*)

let directions piece =
  match piece.kind with
  | Empty -> []
  | King ->
      [
        (* Clockwise *)
        Position.direction 0 1;
        Position.direction 1 1;
        Position.direction 1 0;
        Position.direction 1 (-1);
        Position.direction 0 (-1);
        Position.direction (-1) (-1);
        Position.direction (-1) 0;
        Position.direction (-1) 1;
        (* Castle special handling -- implemented in game.ml *)
        Position.direction (-2) 0;
        Position.direction 2 0;
      ]
  | Queen ->
      [
        Position.direction ~len:7 0 1;
        Position.direction ~len:7 1 1;
        Position.direction ~len:7 1 0;
        Position.direction ~len:7 1 (-1);
        Position.direction ~len:7 0 (-1);
        Position.direction ~len:7 (-1) (-1);
        Position.direction ~len:7 (-1) 0;
        Position.direction ~len:7 (-1) 1;
      ]
  | Rook ->
      [
        Position.direction ~len:7 0 1;
        Position.direction ~len:7 1 0;
        Position.direction ~len:7 0 (-1);
        Position.direction ~len:7 (-1) 0;
      ]
  | Knight ->
      [
        Position.direction 1 2;
        Position.direction 2 1;
        Position.direction 2 (-1);
        Position.direction 1 (-2);
        Position.direction (-1) (-2);
        Position.direction (-2) (-1);
        Position.direction (-2) 1;
        Position.direction (-1) 2;
      ]
  | Bishop ->
      [
        Position.direction ~len:7 1 1;
        Position.direction ~len:7 1 (-1);
        Position.direction ~len:7 (-1) (-1);
        Position.direction ~len:7 (-1) 1;
      ]
  (* Game logic file handles pawn directions. *)
  | Pawn ->
      let dir = if piece.player = Black then -1 else 1 in
      [
        (* Need special consideration if spot is free. *)
        (* Need special consideration if opponent at destination. *)
        Position.direction 0 dir ~len:2;
        (* Need special rules for En Passant *)
        Position.direction 1 dir;
        Position.direction (-1) dir;
      ]
