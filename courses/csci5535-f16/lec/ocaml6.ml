type var = string
type typ =
    Num | Str | List of typ | Unit

type exp =
  | Triv
  | Num of int
  | Nil of typ 
  | Cons of exp * exp
  | InL of exp * typ
  | InR of typ * exp
		    
  | Match of exp * exp * var * var * exp
  | Fold of exp * exp * var * var * exp
  | Map of exp * var * exp * typ
  | Times of exp * exp
  | Plus of exp * exp
  | Var of var

let list     : exp = Cons(Num(1), Cons(Num(2), Cons(Num(3), Nil(Num))))
let test_map : exp = Map( list, "x", Times(Var("x"), Var("x")), Num)
let test_fold : exp = Fold( list, Num(0), "x", "y", Plus(Var("x"), Var("y")))
let test_match : exp = Match( list, InL(Triv, Num), "x", "y", InR(Unit, Var("x")) )

type gamma = (var * typ) list
let exp_typ : gamma -> exp -> typ option = fun g e -> failwith "TODO"
let exp_string : exp -> string = fun e -> failwith "TODO"
let typ_string : typ -> string = fun t -> failwith "TODO"
let is_val : exp -> bool = fun e -> failwith "TODO"
let subst : exp -> var -> exp -> exp = fun e1 x e2 -> failwith "TODO"

let rec exp_typ : gamma -> exp -> typ option =
  fun g e ->
  (match e with
  | Map(el, x, eh, t2) ->
     (match exp_typ g el with 
     | None -> None
     | Some(List(elmt)) -> 
	(match exp_typ ((x,elmt)::g) eh with
	| None -> None
	| Some(t3) -> if t2 = t3 then Some(List(t2)) else failwith "type error"	
	)
     | Some(_) -> failwith "not a list: map expects a list"
     )
  | _ -> failwith "TODO"
  )	

let rec step : exp -> exp = 
  fun e -> 
  match e with
  | Map(el, x, eh, t2) -> 
     if not (is_val el) then Map(step el, x, eh, t2)
     else ( match el with
	  | Nil(t1) -> Nil(t2)
	  | Cons(e1, e2) -> Cons(subst e1 x eh, Map(e2, x, eh, t2))
	  | _ -> failwith "type error: not a list"
	  )
  | _ -> failwith "TODO"
				  

let rec test_pap_rec : exp -> typ option -> exp =
  fun e ot ->
  match ot with
    None -> failwith "bad type"
  | Some(t) -> (
    print_string ("expression:" ^ (exp_string e)^"\n") ;
    print_string ("      type:" ^ (typ_string t)^"\n") ;
    if is_val e then e else
      let e' = step e in
      match exp_typ [] e' with
      | None -> failwith "bad type after step"
      | Some(t') -> if not (t = t') then failwith "bad type after step; does not match"
		    else test_pap_rec e' (Some t)
  )

let rec test_pap : exp -> exp =
  fun e -> test_pap_rec e (exp_typ [] e)

(*
type 'a option = None | Some of 'a
let hd : 'a list -> 'a option =
  fun l -> match l with 
	   | [] -> None
	   | h::t -> Some(h)
 *)

