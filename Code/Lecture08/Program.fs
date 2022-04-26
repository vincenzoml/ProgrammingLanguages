// expressions with identifiers and binding 

// syntax 

type ide = string

type aexp = 
  AEint of int 
| AEplus of (aexp * aexp) 
| AEminus of (aexp * aexp) 
| AElet of (ide * aexp * aexp) 
| AEide of ide

let rec aexp_to_string (e : aexp) =
  match e with
    AEint i -> Printf.sprintf "%d" i
  | AEplus (e1,e2) -> 
    Printf.sprintf "(%s + %s)" (aexp_to_string e1) (aexp_to_string e2)
  | AEminus (e1,e2) -> 
    Printf.sprintf "(%s - %s)" (aexp_to_string e1) (aexp_to_string e2)
  | AElet (v,e1,e2) -> 
    Printf.sprintf "(let %s = %s in %s)" v 
      (aexp_to_string e1) (aexp_to_string e2)
  | AEide v -> v


// error handling 
let unbound_identifier_error ide = 
  failwith (Printf.sprintf "unbound identifier %s" ide)

let negative_natural_number_error () = failwith "natural numbers must be positive or zero"
 
(* semantic domains *)
type eval = int // VERY IMPORTANT: the domain to which identifiers are bound

let eval_to_string : eval -> string =
  fun e -> Printf.sprintf "%d" e
    
type env = ide -> eval // VERY IMPORTANT: this is the environment that associates a "eval" to each defined identifier

let empty = fun v -> unbound_identifier_error v

let bind (en : env) (v : ide) (ev : eval) = // VERY IMPORTANT: the binding operation that computes a new environment out of an environment, an identifier to bind, and the value
  fun v1 ->
    if v1 = v
    then ev
    else en v1

let apply : env -> ide -> eval =
  fun en v -> en v

// denotational semantics 

let rec sem : env -> aexp -> eval =
  fun ev e ->
    match e with
      AEint i -> 
        if i < 0 
        then negative_natural_number_error ()
        else i
    | AEplus (e1,e2) ->
      let s1 = sem ev e1 in
      let s2 = sem ev e2 in
      s1 + s2
    | AEminus (e1,e2) ->
      let s1 = sem ev e1 in
      let s2 = sem ev e2 in
      if s1 >= s2 
      then (s1 - s2) 
      else negative_natural_number_error ()
    | AElet (v,e1,e2) -> 
      let s1 = sem ev e1 in 
      let ev1 = bind ev v s1 in
      sem ev1 e2
    | AEide v -> apply ev v

// test 

let eval : aexp -> unit =
  fun ae ->
    printfn "%s ==> " (aexp_to_string ae);
    try 
      printfn "%s\n" (eval_to_string (sem empty ae))
    with 
      Failure message -> 
      printfn "error: %s\n" message

let l = 
  [ AElet ("x",AEminus (AEint 3,AEint 4),AEint 3);
    AElet ("x",AEminus (AEint 3,AEint 4),AEide "x");
    AElet ("x",AEint 3,AEide "x") ;
    AElet ("x",AEint 3,AEide "y") ;
    AEplus (AElet ("x",AEint 3,AEide "x"),AElet ("y",AEint 5,AEide "y"));
    AEplus (AElet ("x",AEint 3,AEide "x"),AEide "x");
    AElet ("x",AEplus (AEint 3,AEint 4),AEminus (AEide "x",AEplus (AEint 3,AEint 3))) ;
    AElet ("x",AEint 3,AEplus (AEide "x",AEide "x")) ;
    AEplus (AEide "x",AEint 3) ;
    AElet ("x",AEminus (AEint 3,AEint 4),AEide "x") ;
    AElet ("x",AEint 3,AEminus (AEide "x",AEint 4)) ;
    AElet ("x",AEint 3,AElet ("y",AEint 4,AEplus (AEide "x",AEide "y"))) ;
    AElet ("x",AEint 4,AElet ("x",AEint 5,AEide "x")) ]

List.iter eval l

