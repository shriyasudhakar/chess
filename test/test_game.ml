open OUnit2
open Srk_final_3110

let piece_from_string = function
  | "King" -> Piece.King
  | "Queen" -> Piece.Queen
  | "Rook" -> Piece.Rook
  | "Knight" -> Piece.Knight
  | "Bishop" -> Piece.Bishop
  | "Pawn" -> Piece.Pawn
  | _ -> Piece.Empty

let player_from_string = function
  | "White" -> Piece.White
  | "Black" -> Piece.Black
  | _ -> None

let get_position s : Position.position =
  if String.length s = 2 then
    { x = Char.code s.[0] - Char.code 'a'; y = Char.code s.[1] - Char.code '1' }
  else Position.invalid

let create_game ?(turn : Piece.player = White) strings : Game.game =
  let board = Square_matrix.initialize Piece.empty 8 in
  let rec place player piece = function
    | [] -> board
    | s :: t ->
        let next_player = player_from_string s in
        let next_piece = piece_from_string s in
        let position = get_position s in
        let player = if next_player != None then next_player else player in
        let piece = if next_piece != Empty then next_piece else piece in
        if position != Position.invalid then
          Square_matrix.set position (Piece.lookup player piece) board;
        place player piece t
  in
  {
    title = "Chess";
    turn;
    board = place None Empty strings;
    show_welcome = false;
    selected = Position.invalid;
    white = { rook_left = false; king = false; rook_right = false };
    black = { rook_left = false; king = false; rook_right = false };
    past = [];
    future = [];
  }

let verify_pieces (expected : Piece.piece) (found : Piece.piece) =
  assert_equal expected.kind found.kind ~printer:Piece.piece_string;
  assert_equal expected.player found.player ~printer:Piece.player_string

let verify_placements strings (game : Game.game) =
  let rec find player piece = function
    | [] -> ()
    | s :: t ->
        let next_player = player_from_string s in
        let next_piece = piece_from_string s in
        let position = get_position s in
        let player = if next_player != None then next_player else player in
        let piece = if next_piece != Empty then next_piece else piece in
        if position != Position.invalid then
          verify_pieces
            (Piece.lookup player piece)
            (Square_matrix.get position game.board);
        find player piece t
  in
  find None Empty strings

let verify_initial_placements (game : Game.game) =
  verify_placements
    [
      "White";
      "Rook";
      "a1";
      "h1";
      "Knight";
      "b1";
      "g1";
      "Bishop";
      "c1";
      "f1";
      "Queen";
      "d1";
      "King";
      "e1";
      "Pawn";
      "a2";
      "b2";
      "c2";
      "d2";
      "e2";
      "f2";
      "g2";
      "h2";
    ]
    game

let get_sorted_positions moves =
  let positions = List.map (fun move -> Position.to_string move) moves in
  let sorted = List.sort String.compare positions in
  List.fold_left
    (fun acc item -> acc ^ (if acc = "" then "" else ";") ^ item)
    "" sorted

let select_piece source (game : Game.game) =
  let selected = get_position source in
  { game with selected }

let move source destination (game : Game.game) =
  { game with selected = get_position source }
  |> Game.player_move (fun () -> Click (get_position destination))

let validate_moves moves (game : Game.game) =
  let generated_valid_moves =
    game |> Game.get_valid_moves game.selected |> get_sorted_positions
  in
  assert_equal moves generated_valid_moves ~printer:(fun s -> s)

let game_tests =
  "game test suite"
  >::: [
         ( "pawn initial" >:: fun _ ->
           [ "White"; "Pawn"; "h2"; "Rook"; "h5"; "Black"; "Rook"; "g5" ]
           |> create_game |> select_piece "h2" |> validate_moves "h3;h4" );
         ( "pawn capture" >:: fun _ ->
           [ "White"; "Pawn"; "h4"; "Rook"; "h5"; "Black"; "Rook"; "g5" ]
           |> create_game |> select_piece "h4" |> validate_moves "g5" );
         ( "pawn en passant" >:: fun _ ->
           [ "White"; "Pawn"; "e5"; "Black"; "Pawn"; "d5"; "f5" ]
           |> create_game |> select_piece "e5"
           |> fun game ->
           { game with title = "Pawn f7→f5" } |> validate_moves "e6;f6" );
         ( "rook test" >:: fun _ ->
           [ "White"; "Pawn"; "f3"; "Black"; "Rook"; "d5"; "f5" ]
           |> create_game ~turn:Black |> select_piece "f5"
           |> validate_moves "e5;f3;f4;f6;f7;f8;g5;h5" );
         ( "knight test" >:: fun _ ->
           [
             "White"; "Knight"; "h5"; "Rook"; "f6"; "Black"; "Rook"; "f4"; "g4";
           ]
           |> create_game |> select_piece "h5" |> validate_moves "f4;g3;g7" );
         ( "bishop test" >:: fun _ ->
           [ "White"; "Bishop"; "e5"; "Rook"; "c3"; "Black"; "Rook"; "g7" ]
           |> create_game |> select_piece "e5"
           |> validate_moves "b8;c7;d4;d6;f4;f6;g3;g7;h2" );
         ( "queen test" >:: fun _ ->
           [
             "White";
             "Rook";
             "c3";
             "e3";
             "Black";
             "Queen";
             "e5";
             "Rook";
             "b5";
             "g7";
           ]
           |> create_game ~turn:Black |> select_piece "e5"
           |> validate_moves
                "b8;c3;c5;c7;d4;d5;d6;e3;e4;e6;e7;e8;f4;f5;f6;g3;g5;h2;h5" );
         ( "king test" >:: fun _ ->
           [
             "White";
             "Knight";
             "f7";
             "g8";
             "Black";
             "King";
             "e8";
             "Rook";
             "a8";
             "h8";
             "Bishop";
             "b8";
             "Pawn";
             "d7";
           ]
           |> create_game ~turn:Black |> select_piece "e8"
           |> validate_moves "f7;f8" );
         ( "king castle" >:: fun _ ->
           [
             "White";
             "Knight";
             "g4";
             "Black";
             "King";
             "e8";
             "Rook";
             "a8";
             "h8";
             "Bishop";
             "a7";
             "Pawn";
             "d7";
           ]
           |> create_game ~turn:Black |> select_piece "e8"
           |> fun game ->
           { game with white = { game.white with rook_left = true } }
           |> validate_moves "c8;d8;e7;f7;f8;g8" );
         ( "king no queenside" >:: fun _ ->
           [ "White"; "King"; "e1"; "Rook"; "a1"; "h1"; "Black"; "Pawn"; "g3" ]
           |> create_game |> select_piece "e1"
           |> fun game ->
           { game with white = { game.white with rook_left = true } }
           |> validate_moves "d1;d2;e2;f1;g1" );
         ( "king no kingside" >:: fun _ ->
           [ "White"; "King"; "e1"; "Rook"; "a1"; "h1"; "Black"; "Pawn"; "g3" ]
           |> create_game |> select_piece "e1"
           |> fun game ->
           { game with white = { game.white with rook_right = true } }
           |> validate_moves "c1;d1;d2;e2;f1" );
         ( "king no castle" >:: fun _ ->
           [
             "White";
             "Knight";
             "g4";
             "f7";
             "Black";
             "King";
             "f8";
             "Rook";
             "a8";
             "h7";
             "Bishop";
             "a7";
             "d7";
           ]
           |> create_game ~turn:Black |> select_piece "f8"
           |> fun game ->
           { game with black = { game.black with king = true } }
           |> validate_moves "e7;e8;f7;g7;g8" );
         ( "king expose" >:: fun _ ->
           [ "White"; "King"; "e1"; "Pawn"; "f2"; "Black"; "Bishop"; "g3" ]
           |> create_game |> select_piece "f2" |> validate_moves "g3" );
       ]

let equal_boards (game1 : Game.game) (game2 : Game.game) =
  Square_matrix.iterate true
    (fun _ pos (item1 : Piece.piece) ->
      let item2 : Piece.piece = Square_matrix.get pos game2.board in
      assert_equal item1.kind item2.kind;
      assert_equal item1.player item2.player;
      true)
    game1.board

let lst_all_moves_white : Position.position list =
  [
    { x = 7; y = 3 };
    { x = 7; y = 2 };
    { x = 6; y = 3 };
    { x = 6; y = 2 };
    { x = 5; y = 3 };
    { x = 5; y = 2 };
    { x = 4; y = 3 };
    { x = 4; y = 2 };
    { x = 3; y = 3 };
    { x = 3; y = 2 };
    { x = 2; y = 3 };
    { x = 2; y = 2 };
    { x = 1; y = 3 };
    { x = 1; y = 2 };
    { x = 0; y = 3 };
    { x = 0; y = 2 };
  ]

let lst_all_moves_black : Position.position list =
  [
    { x = 7; y = 4 };
    { x = 7; y = 5 };
    { x = 6; y = 4 };
    { x = 6; y = 5 };
    { x = 5; y = 4 };
    { x = 5; y = 5 };
    { x = 4; y = 4 };
    { x = 4; y = 5 };
    { x = 3; y = 4 };
    { x = 3; y = 5 };
    { x = 2; y = 4 };
    { x = 2; y = 5 };
    { x = 1; y = 4 };
    { x = 1; y = 5 };
    { x = 0; y = 4 };
    { x = 0; y = 5 };
  ]

let rec compare_elems lst1 lst2 =
  match lst1 with
  | [] -> true
  | h :: t ->
      if List.exists (fun x -> x = h) lst2 then compare_elems t lst2 else false

let test_all_moves game =
  assert_equal true (compare_elems lst_all_moves_white (Game.all_moves game));
  assert_equal true
    (compare_elems lst_all_moves_black (Game.all_moves (Game.update_turn game)))

let game_moves =
  "game test suite"
  >::: [
         ( "game initial" >:: fun _ ->
           Game.start true |> verify_initial_placements );
         ( "pawn initial" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game |> move "h2" "h3"
           |> verify_placements [ "Empty"; "h2"; "White"; "Pawn"; "h3" ] );
         ( "pawn promotion" >:: fun _ ->
           [ "White"; "Pawn"; "e7" ] |> create_game |> move "e7" "e8"
           |> verify_placements [ "Empty"; "e7"; "White"; "Queen"; "e8" ] );
         ( "king castle and move" >:: fun _ ->
           let g =
             move "e1" "g1"
               (create_game [ "White"; "King"; "e1"; "White"; "Rook"; "h1" ])
           in
           verify_placements
             [
               "Empty";
               "e1";
               "Empty";
               "h1";
               "White";
               "King";
               "g1";
               "White";
               "Rook";
               "f1";
             ]
             g;
           assert_equal true g.white.king );
         ( "rook left move" >:: fun _ ->
           let g = move "a1" "a7" (create_game [ "White"; "Rook"; "a1" ]) in
           assert_equal true g.white.rook_left );
         ( "rook right move" >:: fun _ ->
           let g = move "h1" "h7" (create_game [ "White"; "Rook"; "h1" ]) in
           assert_equal true g.white.rook_right );
         ( "checkmate" >:: fun _ ->
           [ "White"; "Rook"; "a1"; "b7"; "Black"; "King"; "h8" ]
           |> create_game |> move "a1" "a8"
           |> fun game ->
           assert (String.ends_with game.title ~suffix:"Checkmate") );
         ( "stalemate" >:: fun _ ->
           [ "White"; "Queen"; "g1"; "Black"; "King"; "h8" ]
           |> create_game |> move "g1" "g6"
           |> fun game ->
           assert (String.ends_with game.title ~suffix:"Stalemate") );
       ]

let game_player_moves =
  "game player suite"
  >::: [
         ( "help" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game
           |> Game.player_move (fun () -> Key 'h')
           |> fun game ->
           assert game.show_welcome;
           game |> Game.player_move (fun () -> Key 'l') |> fun game ->
           assert (not game.show_welcome) );
         ( "previous none" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game |> fun game ->
           assert_equal 0 (List.length game.past);
           game |> Game.player_move (fun () -> Key 'b') |> fun next ->
           assert (equal_boards game next) );
         ( "forward none" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game |> fun game ->
           assert_equal 0 (List.length game.future);
           game |> Game.player_move (fun () -> Key 'f') |> fun next ->
           assert (equal_boards game next) );
         ( "reset" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game
           |> Game.player_move (fun () -> Key 'r')
           |> verify_initial_placements );
         ( "restart on end" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game ~turn:None
           |> Game.player_move (fun () -> Click { x = 3; y = 2 })
           |> verify_initial_placements );
         ( "forward" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game |> fun game ->
           let prev = game in
           game |> move "h2" "h3" |> fun next ->
           assert (equal_boards prev (List.hd next.past));
           next
           |> Game.player_move (fun () -> Key 'b')
           |> Game.player_move (fun () -> Key 'f')
           |> fun game -> assert (equal_boards game next) );
         ( "back" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game |> move "h2" "h3"
           |> fun game ->
           let next = game in
           game |> Game.player_move (fun () -> Key 'b') |> fun game ->
           assert (equal_boards (List.hd game.future) next) );
         ( "quit" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game |> fun game ->
           assert_raises (Failure "Quit game through keyboard.") (fun () ->
               Game.player_move (fun () -> Key 'q') game) );
         ( "unknown key" >:: fun _ ->
           [ "White"; "Pawn"; "h2" ] |> create_game |> fun prev ->
           prev |> Game.player_move (fun () -> Key '#') |> fun game ->
           assert (equal_boards prev game) );
         ( "mouse click select" >:: fun _ ->
           let pos : Position.position = { x = 0; y = 1 } in
           [ "White"; "Pawn"; "a2" ] |> create_game
           |> Game.player_move (fun () -> Click pos)
           |> fun game -> assert_equal pos game.selected );
         ( "mouse click twice unselects" >:: fun _ ->
           let pos : Position.position = { x = 0; y = 1 } in
           [ "White"; "Pawn"; "a2" ] |> create_game
           |> Game.player_move (fun () -> Click pos)
           |> Game.player_move (fun () -> Click pos)
           |> fun game -> assert_equal Position.invalid game.selected );
         ( "mouse click twice moves" >:: fun _ ->
           let pos : Position.position = { x = 0; y = 1 } in
           [ "White"; "Pawn"; "a2" ] |> create_game
           |> Game.player_move (fun () -> Click pos)
           |> Game.player_move (fun () -> Click { x = 0; y = 2 })
           |> verify_placements [ "Empty"; "a2"; "White"; "Pawn"; "a3" ] );
         ( "mouse click twice no move" >:: fun _ ->
           let pos : Position.position = { x = 0; y = 1 } in
           [ "White"; "Pawn"; "a2" ] |> create_game
           |> Game.player_move (fun () -> Click pos)
           |> Game.player_move (fun () -> Click { x = 0; y = 5 })
           |> verify_placements [ "White"; "Pawn"; "a2" ] );
         ("test all_moves" >:: fun _ -> test_all_moves (Game.start true));
       ]
