// expressions with identifiers and binding 

// syntax 

type ide = string

// AElet represents the fsharp construct of the same name:
// AElet "x" (AEint 3) (AEide "x") 
// is equivalent to: 
// "let x = 3 in x+1" 
// which is equivalent to
// let x = 3
// x+1

type aexp = 
| AEint of int 
| AEplus of (aexp * aexp) 
| AEminus of (aexp * aexp) 
| AElet of (ide * aexp * aexp) // AElet("ciccio",AEplus (AEint 10) (AEint 20),AEplus (AEide "ciccio",AEint 3))
| AEide of ide // AEide "ciccio"

let rec aexp_to_string (e : aexp) =
  match e with
  | AEint i -> Printf.sprintf "%d" i
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
 
// semantic domains 
type eval = int 
type dval = eval // denotable and expressible values coincide

let evalToDval x = x // "converts" a denotable value to an expressible value


let eval_to_string : eval -> string =
  fun e -> Printf.sprintf "%d" e
    
type env = ide -> dval // VERY IMPORTANT: this is the environment that associates a "eval" to each defined identifier

// // What is an environment? Is it memory? NO! Example:
// let f() =
//   let x = [| 1; 2; 3 |]
//   let y = x
//   let x = [|0|] // Commenting this line changes the result
//   x[0] <- 7
//   y[0]

// State vs environment
// let x = 3 in
//  (let x = 4 in x) + x  // The second x has value "3" when using a static environment

let empty : ide -> dval = (fun v -> unbound_identifier_error v)

let bind (en : env) (v : ide) (dv : dval) = // VERY IMPORTANT: the binding operation that computes a new environment out of an environment, an identifier to bind, and the value
  fun v1 -> // The result is an environment
    if v1 = v
    then dv 
    else en v1

let apply : env -> ide -> dval =
  fun en v -> en v

// denotational semantics 

let rec sem (ev : env) (e : aexp) =
  match e with
  | AEint i -> 
      if i < 0 
      then negative_natural_number_error ()
      else (i : eval)
  | AEplus (e1,e2) ->
    let s1 = sem ev e1
    let s2 = sem ev e2
    s1 + s2
  | AEminus (e1,e2) ->
    let s1 = sem ev e1
    let s2 = sem ev e2
    if s1 >= s2 
    then (s1 - s2) 
    else negative_natural_number_error ()
  | AElet (v,e1,e2) -> // The expression below is the same as: sem (bind ev v (sem ev e1)) e2
    let s1 = sem ev e1 
    let ev1 = bind ev v (evalToDval s1) // NOTE: s1 is an "eval"
    sem ev1 e2      
  | AEide v -> apply ev v

// Esercizio: scrivere il termine della sintassi astratta per l'espressione
//
// (let x = 3 in x + 1) - (let y = 2 in y - 1)
//
// E calcolare e stampare il risultato usando la semantica definita sopra, partendo dall'ambiente vuoto
//
// Soluzione:
// let expr = AEminus (( AElet ("x",AEint 3,AEplus(AEide "x",AEint 1)) )   , (AElet ("y",AEint 2,AEminus (AEide "y",AEint 1)))  )
// printfn "%A" (sem empty expr)


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


// Let us add ifthenelse


