(* Language E *)

type ilist = Nil | Cons of (int * ilist)

type num = int (* integer in OCaml *)
type str = string (* string in OCaml *)
type var = string

type typ = 
  Num | Str 
  | Arr of typ * typ
  | Sum of typ * typ
  | Void
  | Prod of typ * typ
  | Unit
  | Nat

(* (Recursive) Datatype in OCaml *)
(* "Context-free grammar" *)
type exp = (* expressions in E *)
  | Lam of var * typ * exp
  | App of exp * exp

  | Num of num
  | Str of str
  | Plus of exp * exp
  | Times of exp * exp
  | Len of exp
  | Cat of exp * exp
  | Let of exp * var * exp
  | Var of var

  | Abort of exp

  | InL of exp * typ
  | InR of typ * exp
  | Case of exp * var * exp * var * exp

  | Pair of exp * exp
	     
  | Zero
  | Succ of exp
  | Rec of exp  (* Z *) * var * var * exp (* S *) * exp (* number *)

let rec string_of_exp e =
  match e with
  | Lam _ -> "lam"
  | App _ -> "app"
  | Zero -> "zero"
  | Succ e -> "succ (" ^ (string_of_exp e) ^ ")"
  | Rec _ -> "rec"
  | _ -> "??"

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
  | Lam (x,t,e1) -> 
     let gamma' = (x,t) :: gamma in
     (
       match (exp_typ gamma' e1) with
       | None -> None
       | Some(t') -> Some(Arr(t, t'))
     )

  | App (e1,e2) -> 
     ( match ((exp_typ gamma e1), (exp_typ gamma e2)) with
     | (Some(Arr(t1, t2))), (Some t3) ->
	if t1 = t3 then Some t2
	else None
     | _ -> None
     )

  | InL (e1, t2) -> 
     ( match exp_typ gamma e1 with
	 Some t1 -> Some (Sum(t1, t2))
       | None -> None
     )

  | InR (t1, e2) -> failwith "TODO"

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
	   | Lam (x,t,e) -> true
	   | Succ e -> is_val e
	   | InL (e,_) -> is_val e
	   | InR (_,e) -> is_val e
	   | Pair(e1, e2) -> (is_val e1) && (is_val e2)
	   | App(e1, e2) -> false

(*	   | Pair (e1, e2) -> is_val e1 && is_val e2 *)
	   | _     -> false

let rec subst : exp -> var -> exp -> exp =
  fun e1 x e2 ->
  match e2 with
  | Case (e20, y1, e21, y2, e22) -> 					
     Case (subst e1 x e20,
	   y1,
	   (if y1 = x then e21 else (subst e1 x e21)),
	   y2,
	   (if y2 = x then e22 else (subst e1 x e22))
	  )

  | App(e21,e22) -> App((subst e1 x e21),(subst e1 x e22))
  | Lam(y, t, e) -> 
     if not (x = y) then Lam(y, t, subst e1 x e)
     else Lam(y, t, e)

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
  fun e -> 
  print_string ("step " ^ (string_of_exp e) ^ "\n") ;
  let e' = match e with
	   | Var _ -> failwith "expression is not closed"
	   | Num _ -> failwith "value does not step"
	   | Str _ -> failwith "value does not step"
			       
	   | App (e1, e2) ->
	      if is_val e1 then 
		if is_val e2 then 
		  match e1 with
		  | Lam(x,_,e1') -> subst e2 x e1'
		  | _ -> failwith "type error"
		else
		  let e2' = step e2 in
		  App(e1, e2')
	      else
		let e1' = step e1 in
		App(e1', e2)
	      
	   | Succ e -> Succ (step e)

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
  in
  print_string ("step " ^ (string_of_exp e) ^ " to " ^ (string_of_exp e') ^ "\n") ;
  e'


let rec all_steps : exp -> exp =
  fun e -> if is_val e then e
	   else all_steps (step e)

(* let _ = *)
(*   let e1  = Plus(Num(1), Plus(Num(2), Num(3))) in *)
(*   let e2  = Plus(Num(1), Num(5)) in *)
(*   let e3 = step e1 in *)
(*   assert (e2 = e3) *)

(* let _ = *)
(*   let e1  = Plus(Plus(Num(1), Num(2)), Num(3)) in *)
(*   let e2  = Plus(Num(3), Num(3)) in *)
(*   let e3 = step e1 in *)
(*   assert (e2 = e3) *)
  
let _ =
  (* TEST 0 FROM CLASS -- Thursday Oct 13 *)
  let e1 = Lam("x",Nat, Lam("x",Nat, Lam("x",Nat, Var("x")))) in
  let e2 = App(App(App(e1, Zero), Succ Zero), Succ (Succ Zero)) in
  let e3 = step (step (step e2)) in
  assert (e3 = (Succ (Succ Zero))) ;

let plus (x:exp) (y:exp) : exp =
  Rec (y, "z1", "z2", Succ(Var("z2")), x)

(* TEST 1 FROM CLASS -- Thursday Oct 13 *)
let test =
  let e : exp = plus (Succ Zero) (Succ(Succ Zero)) in
  let e' = step (step e) in
  assert (e' = (Succ (Succ (Succ Zero))))
  
let plus_lam : exp =
  let e : exp = plus (Var "x") (Var "y") in
  Lam("x", Nat, Lam("y", Nat, e))

(* TEST 2 FROM CLASS -- Thursday Oct 13 *)
let test =
  let e : exp =
    App(App(plus_lam, Succ Zero), Succ(Succ Zero)) in
  let e' = step (step (step (step e))) in
  assert (e' = (Succ (Succ (Succ Zero))))
	
