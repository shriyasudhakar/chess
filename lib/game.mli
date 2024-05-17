type moved = {
  king : bool;
  rook_left : bool;
  rook_right : bool;
}
(** [moved] contains information related to castling (i.e. if a rook has moved).*)

type game = {
  title : string;
  turn : Piece.player;
  board : Piece.piece Square_matrix.t;
  show_welcome : bool;
  selected : Position.position;
  past : game list;
  future : game list;
  white : moved;
  black : moved;
}
(** [game] contains information about the game state. *)

val start : bool -> game
(** [start welcome] gives the game with starting settings and shows the welcome
    screen based on [welcome]. *)

val player_move : (unit -> Display.user_input) -> game -> game
(** [player_move game] gives the next game state after a move is processed in
    [game]. *)

val get_valid_moves : Position.position -> game -> Position.position list
(** [get_valid_moves source game] gets the valid moves of the piece at [source]
    in [game]. Unencapsulated for testing purposes. *)

val all_moves : game -> Position.position list
(** [all_moves game] gets valid moves strictly for checking checkmate and
    stalemate. Unencapsulated for testing purposes *)

val update_turn : game -> game
(** [update_turn] changes player state to opposite (e.g if current player state
    is White then update_turn flips to Black) and vice versa. Unencapsulated for
    testing purposes *)
