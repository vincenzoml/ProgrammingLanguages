// Expressions with let and if then else, booleans 

// syntax 

type ide = string
  
type integer = int

type boolean = True | False
    
type exp = 
  Eint of integer // Es: l'intero "3"
| Eplus of (exp * exp) // 3+3
| Eminus of (exp * exp) // 3-2
| Elet of (ide * exp * exp) // let x = 3 in x+1
| Eide of ide // x
| Ebool of boolean   // Es: il booleano "True"
| Eeql of (exp * exp) // x=2
| Enot of exp
| Eand of (exp * exp)
| Eor of (exp * exp) 
| Eifthenelse of (exp * exp * exp)
		
let rec exp_to_string (e : exp) =
  match e with
    Eint i -> Printf.sprintf "%d" i
  | Eplus (e1,e2) -> 
    Printf.sprintf "(%s + %s)" (exp_to_string e1) (exp_to_string e2)
  | Eminus (e1,e2) -> 
    Printf.sprintf "(%s - %s)" (exp_to_string e1) (exp_to_string e2)
  | Elet (v,e1,e2) -> 
    Printf.sprintf "(let %s = %s in %s)" v (exp_to_string e1) (exp_to_string e2)
  | Eide v -> v
  | Ebool b -> 
    (match b with
      True -> "true"
    | False -> "false")
  | Enot e -> Printf.sprintf "(not %s)" (exp_to_string e)
  | Eand (e1,e2) -> 
    Printf.sprintf "(%s and %s)" (exp_to_string e1) (exp_to_string e2)
  | Eor (e1,e2) -> 
    Printf.sprintf "(%s or %s)" (exp_to_string e1) (exp_to_string e2)
  | Eeql (e1,e2) -> 
    Printf.sprintf "(%s == %s)" (exp_to_string e1) (exp_to_string e2)
  | Eifthenelse (c,e1,e2) -> 
    Printf.sprintf "if %s then (%s) else (%s)" (exp_to_string c) (exp_to_string e1) (exp_to_string e2)


(* error handling *)

let unbound_identifier_error ide = 
  failwith (Printf.sprintf "unbound identifier %s" ide)

let negative_natural_number_error () = failwith "natural numbers must be positive or zero"

let type_error () = failwith "type error"

(* semantic domains *)
    
type eval = Int of int | Bool of bool (* bool è il tipo fsharp! *)
    
let eval_to_string (e : eval) =
  match e with
    Int i -> Printf.sprintf "%d" i
  | Bool b -> if b then "true" else "false"

type dval = eval 

type env = ide -> dval

let empty = fun v -> unbound_identifier_error v

let bind (en : env) (v : ide) (ev : dval) = 
  fun v1 ->
      if v1 = v
      then ev
      else en v1

let apply : env -> ide -> eval =
  fun en v -> en v

(* denotational semantics *)
  
let rec sem : env -> exp -> eval =
  fun ev e ->
    match e with
      Eint i -> 
        if i < 0 
        then negative_natural_number_error ()
        else Int i
    | Eplus (e1,e2) ->
      let s1 = sem ev e1 in
      let s2 = sem ev e2 in
      (match (s1,s2) with 
	(Int i1, Int i2) -> Int (i1 + i2)
      | _ -> type_error ())
    | Eminus (e1,e2) ->
      let s1 = sem ev e1 in
      let s2 = sem ev e2 in
      (match (s1,s2) with
	(Int i1, Int i2) -> 
	  if s1 >= s2 
	  then Int (i1 - i2) 
	  else negative_natural_number_error ()
      | _ -> type_error ())
    | Elet (v,e1,e2) -> 
      let s1 = sem ev e1 in 
      let ev1 = bind ev v s1 in
      sem ev1 e2
    | Eide v -> apply ev v
    | Ebool b -> 
      (match b with 
        True -> Bool true 
      | False -> Bool false)
    | Eeql (e1,e2) ->
      let s1 = sem ev e1 in
      let s2 = sem ev e2 in
      if s1 = s2 
      then Bool true
      else Bool false (* Q: che succede se confronto un intero e un booleano? *)
    | Eand (e1,e2) ->
      let s1 = sem ev e1 in
      let s2 = sem ev e2 in 
      (match (s1,s2) with
        (Bool b1,Bool b2) -> Bool (b1 && b2)
      | _ -> type_error ())
    | Eor (e1,e2) ->
      let s1 = sem ev e1 in
      let s2 = sem ev e2 in 
      (match (s1,s2) with
        (Bool b1,Bool b2) -> Bool (b1 || b2)
      | _ -> type_error ())
    | Enot e ->
      let s = sem ev e in
      (match s with
        Bool b -> Bool (not b)
      | _ -> type_error ())
    | Eifthenelse (c,e1,e2) ->
      let sc = sem ev c in
      (match sc with
        Bool b -> sem ev (if b then e1 else e2)
      | _ -> type_error ())
	

(* test *)

let eval : exp -> unit =
  fun ae ->
    Printf.printf "%s ==> " (exp_to_string ae);
    try 
      Printf.printf "%s\n" (eval_to_string (sem empty ae))
    with 
      Failure message -> 
	Printf.printf "error: %s\n" message

let l = 
  [ Eplus (Eint 2,Eifthenelse (Ebool True,Eint 3,Eint 4));
    Eplus (Eint 2,Eifthenelse (Eeql (Eint 3,Eint 4),Eint 3,Eint 4));
    Eplus (Eint 2,Eifthenelse (Eeql (Eint 3,Eint 3),Eint 3,Ebool False));
    Eplus (Eint 2,Eifthenelse (Eeql (Eint 3,Eint 2),Eint 3,Ebool False));
    Eplus (Eint 2,Eifthenelse (Eor (Eeql (Eint 3,Eint 2),Ebool True),Eint 3,Ebool False));
    Elet ("x",Eifthenelse (Eeql (Eint 3,Eint 3),Ebool True,Eint 3),Eifthenelse (Eide "x",Eint 7,Eint 13));
    Eifthenelse (Ebool True,Eeql (Eint 3,Eint 4),Eeql (Eint 3,Ebool True));
    Eifthenelse (Ebool False,Eeql (Eint 3,Eint 4),Eeql (Eint 3,Ebool True));
  ]
    
let main =
  List.iter eval l

