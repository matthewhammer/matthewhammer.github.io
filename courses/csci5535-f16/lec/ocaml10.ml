type num = int
type var = string
type exp = App of exp * exp
	 | Lam of var * exp
	 | Num of num
	 | Plus of exp * exp

type ec = Hole (* Box / "Empty Evaluation Context" *)
	| App1 of ec * exp
	| App2 of var * exp * ec 
	| Plus1 of ec * exp
	| Plus2 of num * ec


let rec exp_of_ec : ec -> exp -> exp =
  fun ec e ->
  match ec with
  | Hole -> e
  | App1(ec, e2)    -> App(exp_of_ec ec e, e2)
  | App2(x, e1, ec) -> App(Lam(x,e1), exp_of_ec ec e)
  | Plus1(ec, e2)   -> Plus(exp_of_ec ec e, e2) 
  | Plus2(n, ec)    -> Plus(Num n, exp_of_ec ec e)

let rec ec_of_exp : exp -> (ec * exp) = 
  fun e -> 
  match e with
  | Lam _ -> (Hole, e)
  | Num _ -> (Hole, e)

  | App(Lam(var, e'), e2) -> 
     let ec, e3 = ec_of_exp e2 in
     (App2(var, e', ec), e3)

  | App(e1, e2) ->
     let ec, e3 = ec_of_exp e1 in
     (App1(ec, e2), e3)

  | Plus(Num n, e2) ->
     let ec, e3 = ec_of_exp e2 in
     (Plus2(n, ec), e3)

  | Plus(e1, e2) ->
     let ec, e3 = ec_of_exp e1 in
     (Plus1(ec, e2), e3)
     
			   
let rec step : exp -> exp =
  fun e ->
  let ec, e' = ec_of_exp e in
  let e'' = match e' with
  | App(Lam(var, e1), e2) -> failwith "TODO"
  | Plus(Num n1, Num n2)  -> failwith "TODO"
  | _ -> failwith "ERROR"
  in
  exp_of_ec ec e''
				       
	   


