type ilist = Nil
	   | Cons of (int * ilist)

module BubbleSort = struct

  let rec is_sorted : ilist -> bool =
    fun l ->
    match l with
      Nil -> true
    | Cons(i, Nil) -> true
    | Cons(i, Cons(j, l')) -> 
       (i <= j) && (is_sorted (Cons(j, l')))
		     
  let rec bubble : ilist -> ilist =
    fun l ->
    if is_sorted l then l
    else (
      bubble (
	  match l with 
	    Nil -> Nil
	  | Cons(i, Nil) -> Cons(i, Nil)
	  | Cons(i, Cons(j, l')) -> 
	     if j < i then (
	       let b = bubble (Cons(i, l')) in	 
	       Cons(j, b)
	     ) else (
	       let b = bubble (Cons(j, l')) in
	       Cons(i, b)
	     )
	)
    )
end
	   
let rec split : (int -> bool) -> ilist -> (ilist * ilist) =
  fun pred l ->
  match l with
    Nil -> (Nil, Nil)
  | Cons(i,l') -> 
     let (st, sf) = split pred l' in
     if pred i 
     then (Cons(i, st), sf)
     else (st, Cons(i, sf))

let rec filter : (int -> bool) -> (ilist -> ilist) =
  fun pred l ->
  match l with
    Nil -> Nil
  | Cons(i,l') -> 
     let f = filter pred l' in
     if pred i then Cons(i, f)
     else f

let myfilter : ilist -> ilist = filter (fun i -> i < 100)

let rec sum : ilist -> int = fun l ->
  match l with
    Nil         -> 0
  | Cons(i, l') -> i + (sum l')

let rec sum' l s =
  match l with
    Nil         -> s
  | Cons(i, l') -> sum' l' (i+s)

let sum l = sum' l 0

let _ = 
  let l = Cons(1, Cons(2, Cons(3, Nil))) in
  let s = sum l in
  assert (s = 6)

let rec append l m =
  match l with 
    Nil -> m
  | Cons(i, l') -> 
     let p = append l' m in
     Cons(i, p)

let _ = 
  let l1  = Cons(1, Cons(2, Nil)) in
  let l2  = Cons(5, Cons(3, Nil)) in
  let l3  = append l1 l2 in
  let l3' = Cons(1, Cons(2, Cons(5, Cons(3, Nil)))) in
  assert (l3 = l3');
  ()

(* Reverse Version 1 *)
let rec reverse l =
  match l with
    Nil        -> Nil
  | Cons(i,l') ->
     let r' = reverse l' in
     append r' (Cons(i,Nil))

(* Reverse Version 2 *)
let rec reverse' l r =
  match l with
    Nil        -> r
  | Cons(i,l') -> reverse' l' (Cons(i,r))

let reverse l = reverse' l Nil

let _ = 
  let l = Cons(1, Cons(2, Cons(3, Nil))) in
  let r = reverse l in
  let r' = Cons(3, Cons(2, Cons(1, Nil))) in
  assert (r = r');
  ()


(* inductive data type -- recursive datatype *)
type nat (* <<< *) =
    Z
  | S of nat (* <<< *)

(* Version 1: With some type annotations *)
let rec plus : nat -> (nat -> nat) = (
  fun (n:nat) (m:nat) ->
  match n with
    Z     -> m
  | S(n') -> (
    let p = plus n' m in (* top line of inference rule *)
    S(p)
  )
)

(* Version 2: No type annotations; Types are inferred by OCaml compiler. *)
let rec plus n m = match n with
    Z     -> m
  | S(n') -> (
    let p = plus n' m in (* top line of inference rule *)
    S(p)
  )

(* programming with type generics (universally quantified types)  *)
let reassoc : ('a * 'b) -> ('b * 'a) =
  fun pair ->
  let (a,b) = pair in
  (b,a)

(* let _ = print_string "hello world\n" *)
(* let _ = plus (S(Z)) (S(Z)) *)
