type kind =
  | Empty
  | King
  | Queen
  | Rook
  | Knight
  | Bishop
  | Pawn  (** [kind] represents the kind of piece. *)

type player =
  | None
  | White
  | Black  (** [player] represents which player the piece belongs to. *)

type piece = {
  kind : kind;
  player : player;
  image : Graphics.image;
}
(** [piece] contains information about the piece such as the kind, player, etc. *)

val piece_string : kind -> string
(** [piece_string kind] gives the string representation of [kind]. *)

val create : player -> kind -> piece
(** [create] makes a piece given a [player] and a type [kind] for the desired
    piece. *)

val player_string : player -> string
(** [player_string player] gives the string representation of [player]. *)

val other : player -> player
(** [other] gives the other player. *)

val lookup : player -> kind -> piece
(** [lookup player kind] gives a piece with [player] and [kind]. *)

val empty : piece
(** [empty] is a placeholder for a piece with no kind or player. *)

val directions : piece -> Position.direction list
(** [directions piece] gives the possible directions [piece] can move in. *)
