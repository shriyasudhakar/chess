open OUnit2
open Srk_final_3110

let position_tests =
  "position test suite"
  >::: [
         ( "testing origin" >:: fun _ ->
           assert_equal 0 Position.origin.x;
           assert_equal 0 Position.origin.y );
         ( "testing invalid point" >:: fun _ ->
           assert_equal (-1) Position.invalid.x;
           assert_equal (-1) Position.invalid.y );
         ( "testing make position: typical case" >:: fun _ ->
           let p = Position.make_position 3 6 in
           assert_equal 3 p.x;
           assert_equal 6 p.y );
         ( "testing make position: edge case" >:: fun _ ->
           let p = Position.make_position 7 0 in
           assert_equal 7 p.x;
           assert_equal 0 p.y );
         ( "testing valid position: typical case" >:: fun _ ->
           assert_equal true Position.(is_valid (make_position 4 4)) );
         ( "testing valid position: edge case" >:: fun _ ->
           assert_equal true Position.(is_valid origin) );
         ( "testing invalid position" >:: fun _ ->
           assert_equal false Position.(is_valid invalid) );
         ( "testing move position: typical case" >:: fun _ ->
           let move = Position.(move (make_position 3 2) (make_position 2 1)) in
           assert_equal 5 move.x;
           assert_equal 3 move.y );
         ( "testing move position: moving backward" >:: fun _ ->
           let move =
             Position.(move (make_position 3 2) (make_position (-2) (-1)))
           in
           assert_equal 1 move.x;
           assert_equal 1 move.y );
         ( "testing move position: edge case" >:: fun _ ->
           let move = Position.(move (make_position 0 0) (make_position 0 0)) in
           assert_equal 0 move.x;
           assert_equal 0 move.y );
         ( "testing move position: edge case" >:: fun _ ->
           let move = Position.(move (make_position 0 0) (make_position 0 0)) in
           assert_equal 0 move.x;
           assert_equal 0 move.y );
         ( "testing positive direction without providing length argument"
         >:: fun _ ->
           let direction = Position.direction 1 2 in
           assert_equal 1 direction.len;
           assert_equal 1 direction.step.x;
           assert_equal 2 direction.step.y );
         ( "testing positive direction with providing length argument"
         >:: fun _ ->
           let direction = Position.direction ~len:4 1 2 in
           assert_equal 4 direction.len;
           assert_equal 1 direction.step.x;
           assert_equal 2 direction.step.y );
         ( "testing negative direction without providing length argument"
         >:: fun _ ->
           let direction = Position.direction (-1) (-3) in
           assert_equal 1 direction.len;
           assert_equal (-1) direction.step.x;
           assert_equal (-3) direction.step.y );
         ( "testing negative direction with providing length argument"
         >:: fun _ ->
           let direction = Position.direction ~len:3 (-2) (-1) in
           assert_equal 3 direction.len;
           assert_equal (-2) direction.step.x;
           assert_equal (-1) direction.step.y );
         ( "testing to string: typical case" >:: fun _ ->
           let p = Position.(to_string (make_position 1 4)) in
           assert_equal "b5" p );
         ( "testing to string: edge case" >:: fun _ ->
           let p = Position.(to_string (make_position 7 0)) in
           assert_equal "h1" p );
         ( "testing move by row: without length argument" >:: fun _ ->
           let moved = Position.(move_by_row ~max:8 (make_position 0 0)) in
           assert_equal 1 moved.x;
           assert_equal 0 moved.y );
         ( "testing move by row: with length argument" >:: fun _ ->
           let moved =
             Position.(move_by_row ~len:3 ~max:8 (make_position 0 0))
           in
           assert_equal 3 moved.x;
           assert_equal 0 moved.y );
       ]

let all_tests = "all tests" >::: [ position_tests ]
