(* Streams in OCaml.

There are three modules for Streams below.  Each is more general than
the previous:

 - The first module only permits infinite streams of natural numbers.

 - The second module is generic in the stream elements, but still only
permits infinite streams.  Some functions in this module are nearly
identical to those in the previous module, but this module also
permits extra functions for working with streams that use pairing.

 - The final, third module is like the second, except that it permits
streams that are finite, ending in a Nil (like a list).  Unlike a
list, however, these streams are lazy (the tail of a Cons is a
suspended computation, not a value).

 *)

module NatStream = struct
  (* Natural numbers, as an inductive data type: *)
  type nat = Z | S of nat

  let rec plus : nat -> nat -> nat = 
    fun n m -> match n with Z -> m | S(n') -> S(plus n' m)

  let rec times : nat -> nat -> nat =
    fun n m -> failwith "TODO: Should evaluate to n * m, that is, n multiplied by m."

  let rec expon : nat -> nat -> nat =
    fun n m -> failwith "TODO: should evaluate to n ^ m, that is, n raised to the m'th power."

  (* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (* Recall that like natural numbers, lists are inductive and finite.
   They always end in Nil.  By contrast, streams have no Nil; they do
   not end.  *)

  (* Define streams as co-inductive (aka, infinite) sequences: *)
  type stream  = Cons of nat * stream_thk
   and stream_thk = (unit -> stream)

  (* Example: infinite stream of 1's: *)
  let one : nat = S(Z)
  let rec ones : stream = Cons(one, fun (_:unit) -> ones)
  let rec ones : stream = Cons(one, fun _        -> ones)  (* Same meaning as the line above *)

  (* Example: All of the natural numbers; an *infinite* sequence!: *)
  let rec nats  : nat -> stream =  
    fun (n:nat) -> Cons(n, fun _ -> nats (S(n)))

  (* Example: All of the *even* natural numbers; another *infinite* sequence!: *)
  let rec evens  : nat -> stream = 
    fun (n:nat) -> Cons(n, fun (_:unit) -> evens (S(S(n))))
		       
  (* Example: `sum xs ys` is a stream that consists of element-wise sums of the elements of the streams arguments xs and ys: *)
  let rec sums : stream -> stream -> stream =
    fun xs ys ->
    match (xs, ys) with
    | (Cons(n, xst), Cons(m, yst)) -> 
       Cons(plus n m,  fun (_:unit) -> sums (xst ()) (yst ()))

  let rec map : (nat -> nat) -> stream -> stream =
    fun f xs ->
    failwith "TODO NatStream.map" 
    (* Should produce the stream of elements from xs, each mapped by function f. *)

  let rec filter : (nat -> bool) -> stream -> stream =
    fun predicate xs ->
    failwith "TODO NatStream.filter"
    (* Should produce the stream of elements from xs that each satisfy
       the given predicate (the boolean-valued function). *)
	     
  let rec split : (nat -> bool) -> stream -> stream * stream =
    fun predicate xs ->
    failwith "TODO NatStream.split"
   (* Should produce *two* streams of elements from xs: the left
      stream consists of elements that satisfy the predicate; the right
      stream consists of the other elements. *)
	     
  let rec merge : (nat -> nat -> bool) -> stream -> stream -> stream =
    fun choice xs ys ->
    failwith "TODO NatStream.merge"
   (* Should produce a stream that merges xs and ys, according to the
      given choice function.  Given two elements, the choice function
      chooses which element to emit next in the output stream *)
end

module Stream = struct
  (* Define streams as co-inductive (aka, infinite) sequences: *)
  type 'a stream  = Cons of 'a * ('a stream_thk)
   and 'a stream_thk = (unit -> 'a stream)

  let rec map : ('a -> 'b) -> 'a stream -> 'b stream =
    fun f xs ->
    failwith "TODO Stream.map"

  let rec filter : ('a -> bool) -> 'a stream -> 'a stream =
    fun predicate xs ->
    failwith "TODO Stream.filter"
	     
  let rec split : ('a -> bool) -> 'a stream -> ('a stream) * ('a stream) =
    fun predicate xs ->
    failwith "TODO Stream.split"
	     
  let rec merge : ('a -> 'a -> bool) -> ('a stream) -> ('a stream) -> ('a stream) =
    fun choice xs ys ->
    failwith "TODO Stream.merge"


  let rec pairs : ('a stream) -> ('b stream) -> (('a * 'b) stream) =
    fun xs ys ->
    failwith "TODO Stream.pairs"
    (* Should produce a stream that consists of pairs of xs and ys. *)

  let rec unpair : ('a * 'b) stream -> ('a stream) * ('b stream) =
    fun xs ->
    failwith "TODO Stream.unpair"
    (* Should produce two streams that consists of the (left and
       right) projected elements of the input stream. *)
	     
end

module StreamNil = struct
  (* Define streams as co-inductive sequences that *may end* with a Nil, but also may not: *)
  type 'a stream  = Nil | Cons of 'a * ('a stream_thk)
   and 'a stream_thk = (unit -> 'a stream)

  let singleton : 'a -> 'a stream =
    fun a -> Cons(a, fun ()->Nil)

  let rec singletons : ('a stream) -> (('a stream) stream) =
    fun xs -> match xs with
	      | Nil -> Nil
	      | Cons(a, ys) -> 
		 Cons(singleton a, fun () -> singletons (ys ()))

  (* Should produce a stream of streams, such that each element from
     xs becomes a singleton stream (that is, a single-element stream)
     in the output stream. *)

  let rec merge : ('a -> 'a -> bool) -> ('a stream) -> ('a stream) -> ('a stream) =
    fun choice xs ys ->
    match xs, ys with
    | Nil,         Nil         -> Nil
    | Nil,         Cons(_,_)   -> ys
    | Cons(_,_),   Nil         -> xs
    | Cons(x,xst), Cons(y,yst) -> 
       if   choice x y 
       then Cons(x, fun ()-> merge choice (xst())  ys    )
       else Cons(y, fun ()-> merge choice  xs     (yst()))

  (* Should produce a stream that merges xs and ys, according to the
     given choice function.  Given two elements, the choice function
     chooses which element to emit next in the output stream *)
	     
  let sort : ('a -> 'a -> bool) -> 'a stream -> 'a stream =
    fun lte ->
    let rec merge_adjacent 
	    : ('a stream) stream -> ('a stream) stream =
      fun zs -> 
      match zs with
      | Nil -> Nil
      | Cons(s1, rest1) -> (* Invariant: s1 is sorted *)
	 match rest1 () with
	 | Nil -> singleton s1
	 | Cons(s2, rest2) -> (* Invariant: s2 is sorted *)
	    Cons(merge lte s1 s2, (* (merge s1 s2) is sorted *)
		 fun () -> merge_adjacent (rest2()))
    in
    fun xs ->
    let rec sort_rec : ('a stream) stream -> 'a stream =
      fun ys ->
      match ys with
      | Nil -> Nil
      | Cons(s, rest) ->
	 match rest () with
	 | Nil -> (s: 'a stream) (* Invariant: s is sorted *)
	 | Cons(_, _) -> sort_rec (merge_adjacent ys)
    in
    sort_rec (singletons xs)

  (* Given an ordering for elements, less-than-or-equal-to function
     lte, sort should produce a sorted sequence of the elements from
     the input stream.

     Hints: 

     - Use singletons on the input stream to get a stream of streams.

     - On this stream of streams, we want to recursively produce a
       shorter stream of streams, where we merge adjacent streams with
       merge.  I have written the pattern-match above in sort_rec that
       identifies two adjacent streams to merge, s1 and s2.

     - The basecase of this function is when the stream of streams
       consists of a single stream, of all of the the sorted input
       elements.

     Note: Sorting only makes sense for finite streams that eventually end!
     You may wonder why is this is the case. Well, consider if the
     input is infinite: How do you produce the first, smallest element
     of an infinite input stream?  You cannot do so in finite time!

     The benefit of this style of sorting is that it is
     *demand-driven*: The consumer of the output stream determines how
     many output elements are needed, and thus, how many comparisons are
     needed to sort this consumed output.
   *)

end
