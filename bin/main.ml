(** @author Rayhan Khanna (rk696), Shriya Sudhakar (ss3576), Kiran Mitra
   (km936) *)

open Srk_final_3110

(** [game_loop] processes one input from the appropriate player and updates the
    game accordingly. Inputs are from the mouse (clicking on the chess pieces
    and clicking on the destination square) or the keyboard (b for back, f for
    forward, r to reset and q to quit). *)
let rec game_loop game =
  game |> Game.player_move Display.next_input |> game_loop

let _ =
  print_endline
    "Welcome to Chess! All standard chess moves are included as well as \
     Castling (long and short - click square next to castle) and En Passant. \
     Promotion of pawns will always go to queen. Have fun! ";

  try Game.start true |> game_loop with Failure msg -> print_endline msg
