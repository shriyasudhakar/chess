open OUnit2
open Srk_final_3110

let square_matrix_tests =
  "square matrix test suite"
  >::: [
         ( "initialize: empty matrix" >:: fun _ ->
           assert_equal (Array.make_matrix 0 0 0)
             Square_matrix.(initialize 0 0 |> get_array) );
         ( "initialize: integer non-empty matrix" >:: fun _ ->
           assert_equal (Array.make_matrix 5 5 2)
             Square_matrix.(initialize 2 5 |> get_array) );
         ( "initialize: float non-empty matrix" >:: fun _ ->
           assert_equal (Array.make_matrix 5 5 2.)
             Square_matrix.(initialize 2. 5 |> get_array) );
         ( "initialize: chararcter non-empty matrix" >:: fun _ ->
           assert_equal
             (Array.make_matrix 5 5 'a')
             Square_matrix.(initialize 'a' 5 |> get_array) );
         ( "initialize: string non-empty matrix" >:: fun _ ->
           assert_equal
             (Array.make_matrix 5 5 "test")
             Square_matrix.(initialize "test" 5 |> get_array) );
         ( "initialize: boolean non-empty matrix" >:: fun _ ->
           assert_equal
             (Array.make_matrix 5 5 false)
             Square_matrix.(initialize false 5 |> get_array) );
         ( "get: integer non-empty matrix" >:: fun _ ->
           assert_equal 3
             Square_matrix.(initialize 3 5 |> get (Position.make_position 3 3))
         );
         ( "get: float non-empty matrix" >:: fun _ ->
           assert_equal 3.
             Square_matrix.(initialize 3. 5 |> get (Position.make_position 3 3))
         );
         ( "get: character non-empty matrix" >:: fun _ ->
           assert_equal 'a'
             Square_matrix.(
               initialize 'a' 5 |> get (Position.make_position 3 3)) );
         ( "get: string non-empty matrix" >:: fun _ ->
           assert_equal "test"
             Square_matrix.(
               initialize "test" 5 |> get (Position.make_position 3 3)) );
         ( "get: boolean non-empty matrix" >:: fun _ ->
           assert_equal true
             Square_matrix.(
               initialize true 5 |> get (Position.make_position 3 3)) );
         ( "set: integer non-empty matrix" >:: fun _ ->
           let arr = Array.make_matrix 5 5 3 in
           let mat = Square_matrix.initialize 3 5 in
           assert_equal
             (arr.(3).(3) <- 0;
              arr)
             Square_matrix.(
               set (Position.make_position 3 3) 0 mat;
               mat |> get_array) );
         ( "set: float non-empty matrix" >:: fun _ ->
           let arr = Array.make_matrix 5 5 3. in
           let mat = Square_matrix.initialize 3. 5 in
           assert_equal
             (arr.(3).(3) <- 12.;
              arr)
             Square_matrix.(
               set (Position.make_position 3 3) 12. mat;
               mat |> get_array) );
         ( "set: character non-empty matrix" >:: fun _ ->
           let arr = Array.make_matrix 5 5 'b' in
           let mat = Square_matrix.initialize 'b' 5 in
           assert_equal
             (arr.(3).(3) <- 'a';
              arr)
             Square_matrix.(
               set (Position.make_position 3 3) 'a' mat;
               mat |> get_array) );
         ( "set: string non-empty matrix" >:: fun _ ->
           let arr = Array.make_matrix 5 5 "hi" in
           let mat = Square_matrix.initialize "hi" 5 in
           assert_equal
             (arr.(3).(3) <- "test";
              arr)
             Square_matrix.(
               set (Position.make_position 3 3) "test" mat;
               mat |> get_array) );
         ( "set: boolean non-empty matrix" >:: fun _ ->
           let arr = Array.make_matrix 5 5 false in
           let mat = Square_matrix.initialize false 5 in
           assert_equal
             (arr.(3).(3) <- true;
              arr)
             Square_matrix.(
               set (Position.make_position 3 3) true mat;
               mat |> get_array) );
         ( "clone: empty matrix" >:: fun _ ->
           assert_equal
             (Square_matrix.initialize 0 0)
             Square_matrix.(initialize 0 0 |> clone) );
         ( "clone: integer non-empty matrix" >:: fun _ ->
           assert_equal
             (Square_matrix.initialize 1 5)
             Square_matrix.(initialize 1 5 |> clone) );
         ( "clone: float non-empty matrix" >:: fun _ ->
           assert_equal
             (Square_matrix.initialize 1. 5)
             Square_matrix.(initialize 1. 5 |> clone) );
         ( "clone: character non-empty matrix" >:: fun _ ->
           assert_equal
             (Square_matrix.initialize 'a' 5)
             Square_matrix.(initialize 'a' 5 |> clone) );
         ( "clone: string non-empty matrix" >:: fun _ ->
           assert_equal
             (Square_matrix.initialize "test" 5)
             Square_matrix.(initialize "test" 5 |> clone) );
         ( "clone: boolean non-empty matrix" >:: fun _ ->
           assert_equal
             (Square_matrix.initialize true 5)
             Square_matrix.(initialize true 5 |> clone) );
         ( "iterate: integer non-empty matrix" >:: fun _ ->
           let sum acc _ curr = acc + curr in
           assert_equal 32 Square_matrix.(initialize 2 4 |> iterate 0 sum) );
         ( "iterate: float non-empty matrix" >:: fun _ ->
           let sum acc _ curr = acc +. curr in
           assert_equal 32. Square_matrix.(initialize 2. 4 |> iterate 0. sum) );
         ( "iterate: string non-empty matrix" >:: fun _ ->
           let sum acc _ curr = acc ^ curr in
           assert_equal "aaaa"
             Square_matrix.(initialize "a" 2 |> iterate "" sum) );
       ]
