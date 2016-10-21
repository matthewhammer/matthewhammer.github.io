
type 'a list = 
  | Nil 
  | Cons of 'a * 'a list

let lrec : 'b -> ('a -> 'b -> 'b) -> 'a list -> 'b = 
  fun nil cons l ->
(* TYPO: Notice the 'a that should be a 'b on the following line (From Lecture 2016-10-18). *)
(* TYPO: let rec loop : 'a list -> 'a  = *)
         let rec loop : 'a list -> 'b =
    fun l ->
    match l with
      Nil -> nil
    | Cons(h,t) -> cons h (loop t)
  in loop l

let map : ('a -> 'b) -> 'a list -> 'b list =
  fun f l -> lrec Nil (fun h r -> Cons(f h, r)) l

let fold : ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a =
  fun op id l -> lrec id op l

let fold : ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a 
 = fun op id l ->
  let rec loop : 'a list -> 'a =
    fun l ->
    match l with
      Nil -> id
    | Cons(h,t) -> op h (loop t)       
  in loop l

let map : ('a -> 'b) -> 'a list -> 'b list =
  fun f l ->
  let rec loop : 'a list -> 'b list =
    fun l ->	
    match l with
      Nil -> Nil (* << *)
(*    | Cons(h,t) -> Cons(f h, loop t) *)
    | Cons (h,t) -> 
       let p = (h,t) in
       let p' = (f (fst p), loop (snd p)) in
       let (h', t') = p' in
       Cons (h', t')
  in
  loop l

  
(* type 'a option = None | Some of 'a *)

let hd : 'a list -> 'a = 
  fun l -> 
  match l with
    Nil -> failwith "empty list"
  | Cons(h,t) -> h

let hd : 'a list -> 'a option = 
  fun l -> 
  match l with
    Nil -> None
  | Cons(h,_) -> Some h

let tl : 'a list -> ('a list) option =
  fun l ->
  match l with
    Nil -> None
  | Cons(h,t) -> Some t



type 'a tree = Leaf of 'a | Bin of ('a tree) * ('a tree)

let rec trec : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a tree -> 'b =
  fun leaf bin tree -> 
  let rec loop : 'a tree -> 'b =
    fun t -> match t with
	     | Leaf x -> leaf x
	     | Bin(t1, t2) -> bin (loop t1) (loop t2)
  in loop tree

let list_append : 'a list -> 'a list -> 'a list =
  fun l1 l2 -> 
  let f : 'a -> 'a list -> 'a list = 
    (fun h r -> Cons(h, r))
  in
  lrec l2 f l1

let rec list_append : 'a list -> 'a list -> 'a list =
  fun l1 l2 -> match l1 with
	       | Nil -> l2
	       | Cons(h, t) -> let r = (list_append t l2) in
			       Cons(h, r)

let list_of_tree : 'a tree -> 'a list =
  fun t -> trec (fun x -> Cons(x,Nil)) list_append t

let map : ('a -> 'b) -> 'a list -> 'b list =
  fun f l -> lrec Nil (fun h r -> Cons(f h, r)) l

let tree_map : 'a tree -> ('a -> 'b) -> 'b tree =
  fun t f -> trec (fun x -> Leaf(f x)) 
		  (fun t1 t2 -> failwith "TODO") t
  

(*
Next:
 Write tree implementation, from scratch.
 Write map operation.
 Write fold operation.
 Write match operation.

Later:
 Write lazy tree implementation.
 Write map operation.
 Write match operation.
*)
