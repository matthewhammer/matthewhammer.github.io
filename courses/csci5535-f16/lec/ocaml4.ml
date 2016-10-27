
type nat  = Z | S of nat

let rec plus n m =
  match n with
  | Z -> m
  | S(n') -> S (plus n' m)

let rec mult n m =
  match n with
  | Z -> Z
  | S(n') -> plus m (mult n' m) (* = n' * m = (m + m + m + ... m) *)

let rec recnat zero succ n =
  match n with
  | Z -> zero
  | S(n') -> succ (recnat zero succ n')
     
let plus n m = recnat m      (fun n' -> S   (n') ) n
let mult n m = recnat Z      (fun n' -> plus n' m) n
let expn n m = recnat (S(Z)) (fun n' -> mult n' m) n

(*
type even = Z | S of odd
type odd  = S of event
 *)
