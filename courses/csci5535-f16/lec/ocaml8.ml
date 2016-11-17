(* Language E, with "(Assignable) References". 
   See also: PFPL Chapter 35.
*)

type num = int
type str = string
type var = string
type loc = int

type exp = (* expressions in E *)
    Num of num
  | Str of str
  | Plus of exp * exp
  | Times of exp * exp
  | Len of exp
  | Cat of exp * exp
  | Let of exp * var * exp
  | Var of var
(* Ref Stuff: *)
  | Ref of exp
  | Set of exp * exp
  | Get of exp
  | Loc of loc
	    
type typ = Num | Str
	   | Ref of typ

type gamma = (var * typ) list
type store = (loc * exp) list
type storetyp = (loc * typ) list

let rec lookup : ('a * 'b) list -> 'a -> ('b option) =
  fun gamma x -> 
  match gamma with
  | []              -> None
  | (y,t) :: gamma' -> 
     if y = x then (Some t)
     else lookup gamma' x

let gamma_lookup = lookup
let store_lookup = lookup
let storetyp_lookup = lookup

let next_loc : int ref = ref 0

let rec exp_typ : storetyp -> gamma -> exp -> (typ option) =
  fun storetyp gamma e ->
  match e with
  | Num _ -> Some Num
  | Str _ -> Some Str
  | Times _ -> Some Num
  | Cat _ -> Some Str

  | Var x -> gamma_lookup    gamma    x
  | Loc l -> storetyp_lookup storetyp l

  | Get(e) -> (
     match exp_typ storetyp gamma e with
     | Some(Ref(t)) -> Some(t)
     | _            -> None
  )

  | Set(e1, e2) -> (
     match exp_typ storetyp gamma e1 with
     | Some(Ref(t)) -> (
       match exp_typ storetyp gamma e2 with
	 None -> None
       | Some(t') -> (
	 if t = t' then Some(Num) else None
       )
     )
     | _ -> None
  )

  | Ref(e) -> (
    match exp_typ storetyp gamma e with
      None -> None
    | Some(t) -> Some(Ref(t))
  )

  | Let (e1, x, e2) -> (
     let to1 = exp_typ storetyp gamma e1 in
     match to1 with
       Some t1 -> exp_typ storetyp ((x,t1)::gamma) e2
     | None -> None
  )
  | Len e -> (
     let t = exp_typ storetyp gamma e in
     match t with
     Some Str -> Some Num
     | _ -> None
  )
  | Plus (e1, e2) -> 
     let t1 = exp_typ storetyp gamma e1 in
     let t2 = exp_typ storetyp gamma e2 in
     match (t1, t2) with
       (Some Num, Some Num) -> Some Num
     | _ -> None
    
let rec is_val : exp -> bool =
  fun e -> 
  match e with 
  | Num _ -> true
  | Str _ -> true
  | Loc _ -> true
  | _     -> false

let rec subst : exp -> var -> exp -> exp =
  fun e1 x e2 ->
  match e2 with
  | Ref e -> failwith "TODO"
  | Set(e1, e2) -> failwith "TODO"
  | Get(e) -> failwith "TODO"
  | Loc l -> Loc l
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

let rec step : store -> exp -> (store * exp) =
  fun s e -> 
  match e with
  | Var _ -> failwith "expression is not closed"
  | Num _ -> failwith "value does not step"
  | Str _ -> failwith "value does not step"
  | Loc _ -> failwith "value does not step"
		      
  | Ref e -> 
     if is_val e then
       let loc = (List.length s) in
       (((loc, e) :: s), (Loc loc))
     else
       let (s',e') = step s e in
       (s', Ref e')

  | Set(e1, e2) -> 
     if is_val e1 && is_val e2 then
       match e1 with 
       | Loc(l) -> (((l, e2) :: s), Num 0)
       | _ -> failwith "type error"
     else
       failwith "TODO"

  | Get(Loc(l)) -> 
     let e_op = lookup s l in
     ( match e_op with 
       Some(e) -> (s, e)
     | None -> failwith "everything is broken!"
     )

  | Get(e) -> failwith "TODO"
		      
  | Plus (e1,e2) -> 
     if is_val e1 then 
       if is_val e2 then 
	 match (e1, e2) with
	 | ((Num n), (Num m)) -> (s, Num (n+m))
	 | _ -> failwith "type error"
       else
	 let (s', e2') = step s e2 in
	 (s', Plus(e1, e2'))
     else
       let (s', e1') = step s e1 in
       (s', Plus(e1', e2))
	   
  | Len e -> 
     if is_val e then
       match e with
       | Str (str) -> (s, Num (String.length str))
       | _ -> failwith "type error"
     else
       let (s', e') = step s e in
       (s', Len(e'))	  
	  
  | Times _ -> failwith "TODO"
  | Cat _ ->  failwith "TODO"
		       
  | Let (e1, x, e2) ->  
     if is_val e1 then
       (s, subst e1 x e2)
     else
       let (s', e1') = step s e1 in
       (s', Let (e1', x, e2))
	   


  
