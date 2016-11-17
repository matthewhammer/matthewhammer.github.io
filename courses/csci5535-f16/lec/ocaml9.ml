(* Example 1: Reference holding 3: *)
let r3 : int ref = ref 3

(* Another type syntax, 
   which is illegal is OCaml, but used in class: 

   let r3 : ref(int) = ref 3 
*)

(* Example 2: Cyclic list: [1,2,(cycle back to 1)] *)
type clist = Cons of int * (clist ref)
	   | Nil

let r  : clist ref = ref Nil
let c2 : clist     = Cons(2, r)
let c1 : clist     = Cons(1, ref c2)
let () = ( r := c1 )
