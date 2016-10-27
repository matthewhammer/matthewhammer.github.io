(*
Agenda:
-- HW Questions
-- Lecture: Coinductive types (Chapter 15)
*)

type nat = Z | S of nat
type list = Nil | Cons of nat * list
type str_rec  = Cons of nat * str_rec

type str  = Cons of nat * str_thk
and str_thk = (unit -> str)

let one           : nat     = S(Z)
let ones_list     : list    = Cons(one, Cons(one, Nil))
let rec ones_str  : str_rec = Cons(one, ones_str) (* A Hack: Circular "list" *)

let rec ones_str  : str     = Cons(one, fun (_:unit) -> ones_str)

let rec nats_str  : nat -> str = 
  fun (n:nat) -> Cons(n, 
		      fun (_:unit) -> nats_str (S(n)))

let rec even_nats_str  : nat -> str = 
  fun (n:nat) -> Cons(n, 
		      fun (_:unit) -> even_nats_str (S(S(n))))

let nat_of_int : int -> nat = fun i -> failwith "TODO"

let rec another_nats  : int -> str = 
  fun (i:int) -> Cons(nat_of_int i, 
		      fun (_:unit) -> another_nats (i + 1))



let rec my_nats_str : str = nats_str Z
  (* = Cons(Z, Cons(S(Z), Cons(S(S(Z)), Z ...  *)
  
let rec plus : nat -> nat -> nat = fun n m -> failwith "TODO"

let rec sum : list -> list -> list =
  fun xs ys ->
  match (xs, ys) with
    (Nil, Nil) -> Nil
   | (Nil, Cons(m, ys')) -> Cons(m, sum Nil ys')
   | (Cons(m, xs'), Nil) -> Cons(m, sum xs' Nil)
   | (Cons(n, xs'), Cons(m, ys')) -> Cons(plus n m, sum xs' ys')

let rec sum : str -> str -> str =
  fun xs ys ->
  match (xs, ys) with
  | (Cons(n, xst), Cons(m, yst)) -> 
     Cons(plus n m,  fun (_:unit) -> sum (xst ()) (yst ()))	  

(*
type list = 
type str  = 
 *)
