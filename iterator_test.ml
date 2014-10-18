(* tests for iterator.ml module in Problem 2 of CS 3110 Problem Set 4 *)
open Assertions
open Iterator

(* -------------------------------------------------------------- *)
(* LIST ITERATOR TESTING *)
TEST_UNIT "ListIterator_test1" =
  let open ListIterator in
  let iter = create [1;2;3;4;5] in
  assert_true (has_next iter);
  assert_true ((next iter) = 1);
  assert_true (has_next iter);
  assert_true ((next iter) = 2);
  assert_true (has_next iter);
  assert_true ((next iter) = 3);
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 5);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "ListIterator_test2" =
  let open ListIterator in
  let iter = create ["hello";"there"] in
  assert_true (has_next iter);
  assert_true ((next iter) = "hello");
  assert_true (has_next iter);
  assert_true ((next iter) = "there");
  assert_raises (Some NoResult) next iter

TEST_UNIT "ListIterator_test3" =
  let open ListIterator in
  let iter = create [] in
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

(* -------------------------------------------------------------- *)
(* INORDER_TREE_ITERATOR TESTING *)
TEST_UNIT "InorderTreeIterator_test1" =
  let open InorderTreeIterator in
  let tree =     Node(2, 
            Node(7, 
      Node(2,Leaf,Leaf),Node(6,
              Node(5,Leaf,Leaf),Node(11,Leaf,Leaf))),

                              Node(5, Leaf, 
                                    Node(9,
                                  Node(4,Leaf,Leaf),Leaf))) in
  let iter = create tree in
  assert_true (has_next iter);
  assert_true ((next iter) = 2);
  assert_true (has_next iter);
  assert_true ((next iter) = 7);
  assert_true (has_next iter);
  assert_true ((next iter) = 5);
  assert_true (has_next iter);
  assert_true ((next iter) = 6);
  assert_true (has_next iter);
  assert_true ((next iter) = 11);
  assert_true (has_next iter);
  assert_true ((next iter) = 2);
  assert_true (has_next iter);
  assert_true ((next iter) = 5);
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 9);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "InorderTreeIterator_test2" =
  let open InorderTreeIterator in
  let tree = Node(2, Node(7, Leaf,Leaf),Node(8,Leaf,Leaf)) in
  let iter = create tree in
  assert_true (has_next iter);
  assert_true ((next iter) = 7);
  assert_true (has_next iter);
  assert_true ((next iter) = 2);
  assert_true (has_next iter);
  assert_true ((next iter) = 8);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "InorderTreeIterator_test3" =
  let open InorderTreeIterator in
  let tree = Leaf in
  let iter = create tree in
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "InorderTreeIterator_test4" =
  let open InorderTreeIterator in
  let tree = Node (3,Leaf,Leaf) in
  let iter = create tree in
  assert_true (has_next iter);
  assert_true ((next iter) = 3);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "InorderTreeIterator_test5" =
  let open InorderTreeIterator in
  let tree = Node (3,Leaf,Node(4,Leaf,Leaf)) in
  let iter = create tree in
  assert_true (has_next iter);
  assert_true ((next iter) = 3);
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "InorderTreeIterator_test6" =
  let open InorderTreeIterator in
  let tree = Node (3,Node(4,Leaf,Leaf),Leaf) in
  let iter = create tree in
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 3);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

(* -------------------------------------------------------------- *)
(* TAKE_ITERATOR TESTING *)
TEST_UNIT "TAKE_ITERATOR_test1" =
  let module TestModule = TakeIterator (ListIterator) in
  let open TestModule in
  let iter = create 2 (ListIterator.create [1;2;3;4;5]) in
  assert_true (has_next iter);
  assert_true ((next iter) = 1);
  assert_true (has_next iter);
  assert_true ((next iter) = 2);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "TAKE_ITERATOR_test2" =
  let module TestModule = TakeIterator (ListIterator) in
  let open TestModule in
  let iter = create 2 (ListIterator.create [1]) in
  assert_true (has_next iter);
  assert_true ((next iter) = 1);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "TAKE_ITERATOR_test3" =
  let module TestModule = TakeIterator (ListIterator) in
  let open TestModule in
  let iter = create 5 (ListIterator.create [1;2]) in
  assert_true (has_next iter);
  assert_true ((next iter) = 1);
  assert_true (has_next iter);
  assert_true ((next iter) = 2);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

(* n < 0 should not allow any iterations *)
TEST_UNIT "TAKE_ITERATOR_test4" =
  let module TestModule = TakeIterator (ListIterator) in
  let open TestModule in
  let iter = create (-4) (ListIterator.create [1;2;3]) in
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "TAKE_ITERATOR_test5" =
  let module TestModule = TakeIterator (ListIterator) in
  let open TestModule in
  let iter = create 5 (ListIterator.create []) in
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "TAKE_ITERATOR_test6" =
  let module TestModule = TakeIterator (ListIterator) in
  let open TestModule in
  let iter = create 0 (ListIterator.create [1;2]) in
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

(* USING TREE ITERATIONS INSTEAD OF LIST ITERATIONS *)
TEST_UNIT "TAKE_ITERATOR_test7" =
  let module TestModule = TakeIterator (InorderTreeIterator) in
  let open TestModule in
  let iter1 = 
    InorderTreeIterator.create (Node (3,Node(4,Leaf,Leaf),Leaf)) in
  let iter = create 2 iter1 in
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 3);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "TAKE_ITERATOR_test8" =
  let module TestModule = TakeIterator (InorderTreeIterator) in
  let open TestModule in
  let iter1 = 
    InorderTreeIterator.create (Node (3,Node(4,Leaf,Leaf),Leaf)) in
  let iter = create 1 iter1 in
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "TAKE_ITERATOR_test9" =
  let module TestModule = TakeIterator (InorderTreeIterator) in
  let open TestModule in
  let iter1 = 
    InorderTreeIterator.create (Node (3,Node(4,Leaf,Leaf),Leaf)) in
  let iter = create 10 iter1 in
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 3);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "TAKE_ITERATOR_test10" =
  let module TestModule = TakeIterator (InorderTreeIterator) in
  let open TestModule in
  let iter1 = 
    InorderTreeIterator.create Leaf in
  let iter = create 10 iter1 in
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "TAKE_ITERATOR_test11" =
  let module TestModule = TakeIterator (InorderTreeIterator) in
  let open TestModule in
  let tree =     Node(2, 
            Node(7, 
      Node(2,Leaf,Leaf),Node(6,
              Node(5,Leaf,Leaf),Node(11,Leaf,Leaf))),

                              Node(5, Leaf, 
                                    Node(9,
                                  Node(4,Leaf,Leaf),Leaf))) in
  let iter1 = InorderTreeIterator.create tree in
  let iter = create 10 iter1 in
  assert_true (has_next iter);
  assert_true ((next iter) = 2);
  assert_true (has_next iter);
  assert_true ((next iter) = 7);
  assert_true (has_next iter);
  assert_true ((next iter) = 5);
  assert_true (has_next iter);
  assert_true ((next iter) = 6);
  assert_true (has_next iter);
  assert_true ((next iter) = 11);
  assert_true (has_next iter);
  assert_true ((next iter) = 2);
  assert_true (has_next iter);
  assert_true ((next iter) = 5);
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 9);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter


(* -------------------------------------------------------------- *)
(* ITERATOR UTILS FN TESTING *)
TEST_UNIT "iterator_utils_fn_test1" =
  let module TestModule = IteratorUtilsFn (ListIterator) in
  let open TestModule in
  let open ListIterator in
  let iter = create [1;2] in
  advance 2 iter;
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "iterator_utils_fn_test2" =
  let module TestModule = IteratorUtilsFn (ListIterator) in
  let open TestModule in
  let open ListIterator in
  let iter = create [1;2;3;4;5] in
  advance 2 iter;
  assert_true (has_next iter);
  assert_true ((next iter) = 3);
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 5);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "iterator_utils_fn_test3" =
  let module TestModule = IteratorUtilsFn (ListIterator) in
  let open TestModule in
  let open ListIterator in
  let iter = ListIterator.create [] in
  assert_raises (Some NoResult) (advance 2) iter

TEST_UNIT "iterator_utils_fn_test4" =
  let module TestModule = IteratorUtilsFn (ListIterator) in
  let open TestModule in
  let open ListIterator in
  let iter = create [1;5] in
  assert_raises (Some NoResult) (advance 3) iter

TEST_UNIT "iterator_utils_fn_test5" =
  let module TestModule = IteratorUtilsFn (ListIterator) in
  let open TestModule in
  let open ListIterator in
  let iter = create [1;5] in
  advance 2 iter;
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "iterator_utils_fn_test6" =
  let module TestModule = IteratorUtilsFn (ListIterator) in
  let open TestModule in
  let open ListIterator in
  let iter = create [1;5] in
  advance 0 iter;
  assert_true (has_next iter);
  assert_true ((next iter) = 1);
  assert_true (has_next iter);
  assert_true ((next iter) = 5);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

(* negative value shouldn't really be allowed but there
were not descriptions within the skeleton code so I take its 
behavior to be undefined. However, in our case it just doesn't advance
the iterator (ie--behavior is idential to advancing 0 steps) *)
TEST_UNIT "iterator_utils_fn_test7" =
  let module TestModule = IteratorUtilsFn (ListIterator) in
  let open TestModule in
  let open ListIterator in
  let iter = create [1;5] in
  advance (-5) iter;
  assert_true (has_next iter);
  assert_true ((next iter) = 1);
  assert_true (has_next iter);
  assert_true ((next iter) = 5);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

(* using the tree traversal module *)

TEST_UNIT "iterator_utils_fn_test8" =
  let module TestModule = IteratorUtilsFn (InorderTreeIterator) in
  let open TestModule in
  let open InorderTreeIterator in
  let iter = create (Node (3,Node(4,Leaf,Leaf),Leaf)) in
  advance (2) iter;
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "iterator_utils_fn_test9" =
  let module TestModule = IteratorUtilsFn (InorderTreeIterator) in
  let open TestModule in
  let open InorderTreeIterator in
  let iter = create (Node (3,Node(4,Leaf,Leaf),Leaf)) in
  advance 0 iter;
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 3);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "iterator_utils_fn_test10" =
  let module TestModule = IteratorUtilsFn (InorderTreeIterator) in
  let open TestModule in
  let open InorderTreeIterator in
  let iter = create (Node (3,Node(4,Leaf,Leaf),Leaf)) in
  advance (-4) iter;
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 3);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "iterator_utils_fn_test11" =
  let module TestModule = IteratorUtilsFn (InorderTreeIterator) in
  let open TestModule in
  let open InorderTreeIterator in
  let iter = create (Node (3,Node(4,Leaf,Leaf),Leaf)) in
  assert_raises (Some NoResult) (advance 5) iter

TEST_UNIT "iterator_utils_fn_test12" =
  let module TestModule = IteratorUtilsFn (InorderTreeIterator) in
  let open TestModule in
  let open InorderTreeIterator in
  let iter = create Leaf in
  advance 0 iter;
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter

TEST_UNIT "iterator_utils_fn_test13" =
  let module TestModule = IteratorUtilsFn (InorderTreeIterator) in
  let open TestModule in
  let open InorderTreeIterator in
  let tree =     Node(2, 
            Node(7, 
      Node(2,Leaf,Leaf),Node(6,
              Node(5,Leaf,Leaf),Node(11,Leaf,Leaf))),

                              Node(5, Leaf, 
                                    Node(9,
                                  Node(4,Leaf,Leaf),Leaf))) in
  let iter = create tree in
  advance 5 iter;
  assert_true (has_next iter);
  assert_true ((next iter) = 2);
  assert_true (has_next iter);
  assert_true ((next iter) = 5);
  assert_true (has_next iter);
  assert_true ((next iter) = 4);
  assert_true (has_next iter);
  assert_true ((next iter) = 9);
  assert_false (has_next iter);
  assert_raises (Some NoResult) next iter


(* -------------------------------------------------------------- *)
(* RANGE ITERATOR FN TESTING *)











(* Summary statement for test cases *)
let () = Pa_ounit_lib.Runtime.summarize()