(* Language E *)

type ilist = Nil | Cons of (int * ilist)

type num = int (* integer in OCaml *)
type str = string (* string in OCaml *)
type var = string

(* (Recursive) Datatype in OCaml *)
(* "Context-free grammar" *)
type exp = (* expressions in E *)
    Num of num
  | Str of str
  | Plus of exp * exp
  | Times of exp * exp
  | Len of exp
  | Cat of exp * exp
  | Let of exp * var * exp
  | Var of var
	     
  | Zero
  | Succ of exp
  | Rec of exp (* Z *) * var * var * exp (* S *) * exp (* number *)
  | Lam of var * (* typ * *) exp
  | App of exp * exp

type typ = 
  Num | Str

type gamma = (var * typ) list

let rec lookup : gamma -> var -> (typ option) =
  fun gamma x -> 
  match gamma with
  | []              -> None
  | (y,t) :: gamma' -> 
     if y = x then (Some t)
     else lookup gamma' x

let rec exp_typ : gamma -> exp -> (typ option) =
  fun gamma e ->
  match e with
  | Num _ -> Some Num
  | Str _ -> Some Str
  | Times _ -> failwith "similar"
  | Cat _ -> failwith "similar"
  | Var x -> lookup gamma x
  | Let (e1, x, e2) -> (
     let to1 = exp_typ gamma e1 in
     match to1 with
       Some t1 -> exp_typ ((x,t1)::gamma) e2
     | None -> None
  )
  | Len e -> (
     let t = exp_typ gamma e in
     match t with
     Some Str -> Some Num
     | _ -> None
  )
  | Plus (e1, e2) -> 
     let t1 = exp_typ gamma e1 in
     let t2 = exp_typ gamma e2 in
     match (t1, t2) with
       (Some Num, Some Num) -> Some Num
     | _ -> None
	      


    
let rec is_val : exp -> bool =
  fun e -> match e with 
	   (* Pattern -> Arm/Body/RHS *)
	   | Num _ -> true
	   | Str _ -> true
	   | Zero  -> true
	   | Succ e -> is_val e
(*	   | Pair (e1, e2) -> is_val e1 && is_val e2
	   | InL e -> is_val e
	   | InR e -> is_val e *)
	   | _     -> false

let rec subst : exp -> var -> exp -> exp =
  fun e1 x e2 ->
  match e2 with
  | App(e21,e22) -> App((subst e1 x e21),(subst e1 x e22))
  | Lam(y, e) -> 
     if x <> y then Lam(y, subst e1 x e)
     else Lam(y, e)

  | Zero -> Zero
  | Succ e -> Succ (subst e1 x e)
  | Rec(e0, y1,y2, es, e) ->
     assert (y1 <> y2) ;
     if x <> y1 && (not (x = y2)) then
       Rec(subst e1 x e0,
	   y1, y2,
	   subst e1 x es,
	   subst e1 x e)
     else
       Rec(subst e1 x e0,
	   y1, y2,
	   es,
	   subst e1 x e)

  | Num n -> Num n
  | Str s -> Str s
  | Var y -> 
     if x = y then e1 
     else Var y
  | Plus(e2', e2'') -> 
     let e2'  = subst e1 x e2' in
     let e2'' = subst e1 x e2'' in
     Plus(e2', e2'')
  | Times(e2', e2'') -> failwith "similar"
  | Cat(e2', e2'') -> failwith "similar"
  | Len(e2') -> failwith "similar"
  | Let(e2', y, e2'') -> 
     let e2' = subst e1 x e2' in
     if y = x then 
       Let(e2', y, e2'')
     else
       Let(e2', y, (subst e1 x e2''))

let rec step : exp -> exp =
  fun e -> match e with
	   | Var _ -> failwith "expression is not closed"
	   | Num _ -> failwith "value does not step"
	   | Str _ -> failwith "value does not step"

	   | Rec(e0, x,y,es, e) ->
	      if is_val e then (
		match e with 
		| Zero -> e0
		| Succ(e') (* this is e *) -> 
		   let r = Rec(e0, x,y,es, e') in
		   let es = subst e' x es in
		   let es = subst r  y es in
		   es
	      )
	      else (
		let e' = step e in
		Rec(e0, x,y,es, e')
	      )

	   | Plus (e1,e2) -> 
	      if is_val e1 then 
		if is_val e2 then 
		  match (e1, e2) with
		  | ((Num n), (Num m)) -> Num (n+m)
		  | _ -> failwith "type error"
		else
		  let e2' = step e2 in
		  Plus(e1, e2')
	      else
		let e1' = step e1 in
		Plus(e1', e2)

	   | Len e -> 
	      if is_val e then
		match e with
		| Str (s) -> Num (String.length s)
		| _ -> failwith "type error"
	      else
		let e' = step e in
		Len(e')


	   | Times _ -> failwith "TODO"
	   | Cat _ ->  failwith "TODO"
	   
	   | Let (e1, x, e2) ->  
	      if is_val e1 then
		subst e1 x e2
	      else
		let e1' = step e1 in
		Let (e1', x, e2)

let rec all_steps : exp -> exp =
  fun e -> if is_val e then e
	   else all_steps (step e)

let _ =
  let e1  = Plus(Num(1), Plus(Num(2), Num(3))) in
  let e2  = Plus(Num(1), Num(5)) in
  let e3 = step e1 in
  assert (e2 = e3)

let _ =
  let e1  = Plus(Plus(Num(1), Num(2)), Num(3)) in
  let e2  = Plus(Num(3), Num(3)) in
  let e3 = step e1 in
  assert (e2 = e3)
  
