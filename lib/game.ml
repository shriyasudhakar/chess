open Piece
open Position

type moved = {
  king : bool;
  rook_left : bool;
  rook_right : bool;
}

type game = {
  title : string;
  turn : player;
  board : piece Square_matrix.t;
  show_welcome : bool;
  selected : position;
  past : game list;
  future : game list;
  white : moved;
  black : moved;
}

(* helper for starting_board *)

let rec setter_helper color pieces row_index x chess_board =
  match pieces with
  | [] -> chess_board
  | piece :: t ->
      chess_board
      |> Square_matrix.set { x; y = row_index } (Piece.lookup color piece);
      setter_helper color t row_index (x + 1) chess_board

let init_color_piece color row_index pawn_index chess_board =
  let layout = [ Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook ] in
  chess_board
  |> setter_helper color layout row_index 0
  |> setter_helper color (List.init 8 (fun _ -> Pawn)) pawn_index 0

(* end of helpers for starting_board *)

let starting_board =
  Square_matrix.initialize Piece.empty 8
  |> init_color_piece White 0 1 |> init_color_piece Black 7 6

let welcome_image = Bitmap_file.load "images/Welcome.bmp"

let start welcome =
  {
    title = "Chess";
    turn = White;
    board = starting_board;
    past = [];
    future = [];
    show_welcome = welcome;
    selected = invalid;
    white = { rook_left = false; king = false; rook_right = false };
    black = { rook_left = false; king = false; rook_right = false };
  }

let get_piece position game = Square_matrix.get position game.board

let draw_tile ?(highlight = false) position game =
  Display.draw_tile ~highlight (get_piece position game).image position

let clone game =
  {
    game with
    board = Square_matrix.clone game.board;
    past = game :: game.past;
    future = [];
  }

(** [record_move] tracks King and Rook pieces to disallow castle. *)
let record_move source game =
  let update moved = function
    | 0 -> { moved with rook_left = true }
    | 4 -> { moved with king = true }
    | 7 -> { moved with rook_right = true }
    | _ -> moved
  in
  let player =
    match (game.turn, source.y) with
    | White, 0 -> White
    | Black, 7 -> Black
    | _ -> None
  in
  match player with
  | White -> { game with white = update game.white source.x }
  | Black -> { game with black = update game.black source.x }
  | _ -> game

let update_title source destination game =
  let captured = get_piece destination game in
  let piece = get_piece source game in
  {
    game with
    title =
      Printf.sprintf "%s %s %s→%s%s"
        (player_string piece.player)
        (piece_string piece.kind)
        (Position.to_string source)
        (Position.to_string destination)
        (if captured.kind != Empty then
           " captured " ^ piece_string captured.kind
         else "");
  }

(** [update_move] moves pieces on the board and considers special moves such as
    castle or en passant. *)
let update_move source destination game =
  let piece = get_piece source game in
  let updated_game =
    match piece.kind with
    | King when abs (source.x - destination.x) = 2 ->
        let king_side = source.x < destination.x in
        let rook_pos = { x = (if king_side then 7 else 0); y = source.y } in
        let rook_end =
          {
            x = (if king_side then -1 else 1) + destination.x;
            y = destination.y;
          }
        in
        Square_matrix.set rook_end (get_piece rook_pos game) game.board;
        Square_matrix.set rook_pos empty game.board;
        {
          game with
          title =
            (player_string piece.player ^ if king_side then " 0-0" else " 0-0-0");
        }
    | Pawn when destination.y = 0 || destination.y = 7 ->
        Square_matrix.set source (Piece.lookup game.turn Queen) game.board;
        { game with title = game.title ^ " promoted" }
    | Pawn
      when destination.x != source.x
           && (get_piece destination game).kind = Empty ->
        Square_matrix.set { x = destination.x; y = source.y } empty game.board;
        { game with title = game.title ^ " En Passant" }
    | _ -> game
  in
  let piece = get_piece source updated_game in
  Square_matrix.set destination piece updated_game.board;
  Square_matrix.set source empty updated_game.board;
  updated_game

let update_turn game =
  {
    game with
    turn =
      (match game.turn with
      | White -> Black
      | Black -> White
      | _ -> None);
  }

(** get starting position of piece to move. *)
let rec is_castle_path_valid position dir game =
  match dir.len with
  | 0 -> true
  | _ ->
      List.mem (get_piece position game).kind [ King; Rook; Empty ]
      (* && check if game is not attacking this position *)
      && is_castle_path_valid
           (Position.move position dir.step)
           { dir with len = dir.len - 1 }
           game

let get_piece_starting_position source game =
  if is_valid source && (get_piece source game).player = game.turn then source
  else invalid

let allow_king_move src dst game =
  let dist = dst.x - src.x in
  abs dist <= 1
  ||
  (* castle move *)
  let moves = if game.turn == White then game.white else game.black in
  let row = if game.turn == White then 0 else 7 in
  let rook_moved = if dist < 0 then moves.rook_left else moves.rook_right in
  let dir =
    if dist < 0 then { len = 5; step = { x = -1; y = 0 } }
    else { len = 4; step = { x = 1; y = 0 } }
  in
  (not moves.king) && (not rook_moved) && src.x = 4 && src.y = row
  && is_castle_path_valid src dir (game |> clone |> update_turn)

let allow_piece_move source dst game =
  is_valid dst
  &&
  match (get_piece source game).kind with
  | King -> allow_king_move source dst game
  | Pawn when source.x = dst.x ->
      (* Pawn moves straight ahead to open positions. *)
      (get_piece dst game).player = None
      &&
      let len = dst.y - source.y in
      abs len = 1 || (len = 2 && dst.y = 3) || (len = -2 && dst.y = 4)
  | Pawn ->
      (* Take down opponent *)
      (get_piece dst game).player = other game.turn
      ||
      (* En Passant *)
      let alongside = { x = dst.x; y = source.y } in
      let start =
        Position.move alongside
          { x = 0; y = (if source.y < dst.y then 2 else -2) }
      in
      String.ends_with
        ~suffix:
          (Printf.sprintf "Pawn %s→%s" (Position.to_string start)
             (Position.to_string alongside))
        game.title
  | _ -> true

let rec get_valid_moves_one_direction source position dir game =
  match dir.len with
  | 0 -> []
  | _ ->
      let new_position = Position.move position dir.step in
      if allow_piece_move source new_position game then
        match (get_piece new_position game).player with
        | None ->
            new_position
            :: get_valid_moves_one_direction source new_position
                 { dir with len = dir.len - 1 }
                 game
        | p -> if p = game.turn then [] else [ new_position ]
      else []

let rec get_valid_moves_all_directions directions source game =
  match directions with
  | [] -> []
  | h :: t ->
      get_valid_moves_one_direction source source h game
      @ get_valid_moves_all_directions t source game

let rec contains lst position =
  match lst with
  | [] -> false
  | h :: t -> if h = position then true else contains t position

let get_king_position game player =
  let king_pos = ref { x = -1; y = -1 } in
  for y = 0 to 7 do
    for x = 0 to 7 do
      let piece = Square_matrix.get { x; y } game.board in
      if piece.kind = King && piece.player = player then king_pos := { x; y }
    done
  done;
  !king_pos

(* accumulates all of valid positions to help figure out if king in check *)
let king_check_position_lst game =
  let opp_player = game.turn in
  let opp_pieces = ref [] in
  for y = 0 to 7 do
    for x = 0 to 7 do
      let piece = Square_matrix.get { x; y } game.board in
      if piece.player = opp_player then
        opp_pieces :=
          get_valid_moves_all_directions (directions piece) { x; y } game
          @ !opp_pieces
    done
  done;
  !opp_pieces

let is_king_in_check game =
  let game2 = game |> clone |> update_turn in
  let king_pos = get_king_position game2 game.turn in
  contains (king_check_position_lst game2) king_pos

let rec get_valid_moves_remove_check source lst game acc =
  match lst with
  | [] -> acc
  | h :: t ->
      let clone_game = game |> clone |> update_move source h in
      if is_king_in_check clone_game then
        get_valid_moves_remove_check source t game acc
      else get_valid_moves_remove_check source t game (h :: acc)

let get_valid_moves source game =
  if Position.is_valid source then
    let piece = get_piece source game in
    if piece.player = game.turn then
      let pos_lst =
        get_valid_moves_all_directions (directions piece) source game
      in
      get_valid_moves_remove_check source pos_lst game []
    else []
  else []

let all_moves game =
  let moves = ref [] in
  for y = 0 to 7 do
    for x = 0 to 7 do
      let piece = Square_matrix.get { x; y } game.board in
      if piece.player = game.turn then
        moves := get_valid_moves { x; y } game @ !moves
    done
  done;
  !moves

let checkmate game =
  if is_king_in_check game then List.length (all_moves game) = 0 else false

let stalemate game =
  if not (is_king_in_check game) then List.length (all_moves game) = 0
  else false

let draw game =
  let moves = get_valid_moves game.selected game in
  let iter_draw _ position _ =
    draw_tile
      ~highlight:(position = game.selected || List.mem position moves)
      position game
  in
  Display.resize_window ();
  Display.set_title game.title;
  Display.auto_synchronize false;
  Square_matrix.iterate () iter_draw game.board;
  if game.show_welcome then Display.draw_image_centered welcome_image;
  Display.auto_synchronize true;
  game

let update_end game =
  if checkmate game then
    { game with title = game.title ^ " Checkmate"; turn = None }
  else if stalemate game then
    { game with title = game.title ^ " Stalemate"; turn = None }
  else if is_king_in_check game then { game with title = game.title ^ " Check" }
  else game

let move source destination game =
  let piece = get_piece source game in
  match piece.player with
  | None -> game
  | _ ->
      game |> clone |> record_move source
      |> update_title source destination
      |> update_move source destination
      |> update_turn |> update_end

let player_move (inputs : unit -> Display.user_input) game =
  let game = draw game in
  let game = { game with show_welcome = false } in
  let game_unselected = { game with selected = invalid } in
  match inputs () with
  | Click pos ->
      (* End of game, restart a new one. *)
      if game.turn = None then start false
      else if game.selected = invalid then
        (* If no piece selected to move, select the piece. *)
        { game with selected = get_piece_starting_position pos game }
      else if is_valid pos && List.mem pos (get_valid_moves game.selected game)
      then (
        let next = move game.selected pos game_unselected in
        Printf.printf "%s\n%!" next.title;
        next)
      else game_unselected
  | Key key -> (
      match Char.lowercase_ascii key with
      | 'b' -> (
          match game.past with
          | h :: _ -> { h with future = game :: h.future }
          | _ -> game)
      | 'f' -> (
          match game.future with
          | h :: _ -> h
          | _ -> game)
      | 'h' -> { game with show_welcome = true }
      | 'r' -> start false
      | 'q' -> failwith "Quit game through keyboard."
      | _ -> game)
