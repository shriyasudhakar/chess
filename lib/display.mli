type user_input =
  | Click of Position.position
  | Key of char
      (** [user_input] are the kinds of user input the display can accept. *)

val initialize : unit -> unit
(** [initialize] initializes the display with starting settings. *)

val resize_window : unit -> unit
(** [resize_window] resizes the display to the starting size. *)

val set_title : string -> unit
(** [set_title str] makes the title of the display [str]. *)

val draw_image_centered : Graphics.image -> unit
(** [draw_image_centered image] puts [image] in the center of the display. *)

val auto_synchronize : bool -> unit
(** [auto_synchronize sync] says whether or not to update the display based on
    [sync]. Turning off auto-synchronization prevents flickers and random
    updates while performing background operations. *)

val draw_tile : ?highlight:bool -> Graphics.image -> Position.position -> unit
(** [draw_tile highlight image position] draws a tile of the board with [image]
    (of a piece) at [position]. [highlighted] says whether or not the tile is
    highlighted (i.e. to indicate possible moves). *)

val next_input : unit -> user_input
(** [next_input] gets the next input to the display. *)
