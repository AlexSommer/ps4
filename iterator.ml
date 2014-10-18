module type ITERATOR = sig
  type 'a t
  exception NoResult

  (* returns: true if there are more results to yield,
   *   otherwise returns false. *)
  val has_next: 'a t -> bool

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  val next: 'a t -> 'a
end

module type LIST_ITERATOR = sig
  include ITERATOR
  (* parameters:  a list l
   * returns:  an iterator that will yield the elements of l,the
   *   each exactly once, in the order that they appear in l,
   *   starting with the head.  *)
  val create: 'a list -> 'a t
end

module ListIterator : LIST_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult

  let create (lst:'a list) : 'a t =
    ref lst

  let has_next (iter:'a list ref) : bool =
    (!iter <> [])

  let next (iter:'a list ref) : 'a =
    match !iter with
    | [] -> raise NoResult
    | hd::tl -> 
        iter:=tl;
        hd
end



type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

module type INORDER_TREE_ITERATOR = sig
  include ITERATOR
  (* parameters:  a tree t
   * returns:  an iterator that will yield the elements of t,
   *   each exactly once, in the order that would be produced
   *   by an in-order traversal of t. *)
  val create: 'a tree -> 'a t
end

module InorderTreeIterator : INORDER_TREE_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult

  let create (t:'a tree) : 'a t =
    let rec helper t =
      match t with
      | Leaf -> []
      | Node (data,left,right) -> 
          (helper left) @ [data] @ (helper right) in
    ref (helper t)

  let next (iter:'a list ref) : 'a =
    match !iter with
    | [] -> raise NoResult
    | hd::tl -> 
        iter:=tl;
        hd

  let has_next (iter:'a list ref) : bool =
    (!iter <> [])
end


module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR

  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)
  val create: int -> 'a I.t -> 'a t
end

module TakeIterator : TAKE_ITERATOR = functor (I : ITERATOR) -> struct
  type 'a t = 'a I.t
  exception NoResult

  let counter = ref 0

  let create (n:int) (iter:'a I.t) : 'a t =
    counter:=n; iter

  let has_next (iter:'a I.t) : bool =
    (!counter > 0) && (I.has_next iter)

  let next (iter:'a I.t) : 'a =
    if ((!counter > 0) && (I.has_next iter)) then 
      begin
      counter:= !counter - 1;
      I.next iter
      end
    else
      raise NoResult
end


module IteratorUtilsFn (I : ITERATOR) = struct
  open I
  (* effects: causes iter to yield n results, ignoring
   *   those results.  Raises NoResult if iter raises NoResult itself.*)
  let advance (n: int) (iter: 'a I.t) : unit =
    for i=n downto 1 do 
      (next iter);
    done

  (* returns: the final value of the accumulator after
   *   folding f over all the results returned by i,
   *   starting with acc as the initial accumulator.
   * effects: causes i to yield all its results. *)
  let rec fold (f : ('a -> 'b -> 'a)) (acc : 'a) (iter: 'b I.t) : 'a =
    if (has_next iter) then 
      fold f (f acc (next iter)) iter
    else 
      acc
end

module type RANGE_ITERATOR = functor (I : ITERATOR) -> sig
  include ITERATOR

  (* parameters: integers n and m and an iterator i
   * returns: an iterator that behaves the way i would
   *   on the nth through mth calls to next, and
   *   afterwards raises NoResult.
   *
   *   If n > m the resulting iterator should always raise NoResult.
   *)
  val create : int -> int -> 'a I.t -> 'a t
end

module RangeIterator : RANGE_ITERATOR = functor (I : ITERATOR) -> struct
  exception NoResult
  module IteratorAdvance = (IteratorUtilsFn (I))
  module IteratorEnforce = (TakeIterator (I))
  type 'a t = 'a IteratorEnforce.t

  let create (n:int) (m:int) (iter:'a I.t) : 'a t =
    let counter = ref (n-1) in
    while ((I.has_next iter) && (!counter > 0)) do
      counter:=!counter - 1;
      IteratorAdvance.advance 1 iter;
    done;
    IteratorEnforce.create (m-(n-1)) iter

  let has_next (iter:'a t) : bool =
    IteratorEnforce.has_next iter

  let next (iter:'a t) : 'a =
    try
      IteratorEnforce.next iter
    with IteratorEnforce.NoResult -> raise NoResult
end