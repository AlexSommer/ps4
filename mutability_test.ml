open Assertions
open Mutability

(* -------------------------------------------------------------------------- *)
(*Tests for count_up_from *)
TEST_UNIT "count_up_from_test1" = 
	let f = count_up_from 0 1 in
	assert_true (f () = 0 && f () = 1 && f () = 2)


TEST_UNIT "count_up_from_test2" = 
	let f = count_up_from 0 0 in
	assert_true (f () = 0 && f () = 0 && f () = 0)

TEST_UNIT "count_up_from_test3" = 
	let f = count_up_from 5 (-2) in
	assert_true (f () = 5 && f () = 3 && f () = 1)


(* -------------------------------------------------------------------------- *)
(* Tests for tabulate *) 
TEST_UNIT "tabulate_test1" = 
	let f x = x in
	assert_true (tabulate f 5 = [|0;1;2;3;4|])

TEST_UNIT "tabulate_test2" = 
	let f x = x in
	assert_true (tabulate f 0 = [||])

TEST_UNIT "tabulate_test3" = 
	let f x = x*x in
	assert_true (tabulate f 3 = [|0;1;4|])

 TEST_UNIT "tabulate_test4" = 
	let f x = x in
	assert_raises (Some (Invalid_argument "Array.make")) (tabulate f) (-2)


(* -------------------------------------------------------------------------- *)
(*Tests for fold_left_imp *)
TEST_UNIT "fold_left_imp_test1" = 
	assert_true (fold_left_imp ( + ) 0 [1;2;3;4;5] = 15 ); 

TEST_UNIT "fold_left_imp_test2" = 
	assert_true (fold_left_imp ( * ) 1 [] = 1 ); 

TEST_UNIT "fold_left_imp_test3" = 
	assert_true (fold_left_imp ( * ) 1 [5] = 5 ); 

TEST_UNIT "fold_left_imp_test4" = 
	assert_true (fold_left_imp ( * ) 0 [5] = 0 ); 

TEST_UNIT "fold_left_imp_test5" = 
	assert_true (fold_left_imp ( + ) 7 [1;2;3;4;5] = 22 ); 

(* -------------------------------------------------------------------------- *)
(* Test for Zardoz *)
TEST_UNIT "Zardoz_test1" = 
	assert_false (List.map zardoz (List.rev lst) = 
		List.rev (List.map zardoz lst)) 