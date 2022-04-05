// expressions over _natural_ numbers 


// syntactic domains 

type aexp = 
  AEint of int 
| AEplus of (aexp * aexp) 
| AEminus of (aexp * aexp) 
| AEtimes of (aexp * aexp)

let rec aexp_to_string : aexp -> string = 
  fun e ->
    match e with
      AEint i -> Printf.sprintf "%d" i
    | AEplus (e1,e2) -> Printf.sprintf "(%s + %s)" (aexp_to_string e1) (aexp_to_string e2)
    | AEminus (e1,e2) -> Printf.sprintf "(%s - %s)" (aexp_to_string e1) (aexp_to_string e2)
    | AEtimes (e1,e2) -> Printf.sprintf "(%s * %s)" (aexp_to_string e1) (aexp_to_string e2)
      
// semantic domains 
type eval = int
  
let eval_to_string : eval -> string =
  fun e -> Printf.sprintf "%d" e

// error handling 
let negative_natural_number_error () = failwith "natural numbers must be positive or zero"
  
  
// denotational semantics 
let rec sem : aexp -> eval =
  fun ae ->
    match ae with
      AEint i -> 
        if i < 0 
        then negative_natural_number_error ()
        else i
    | AEplus (e1,e2) ->  (* Q: devo controllare che s1 e s2 siano non negativi? *)
      let s1 = sem e1 in
      let s2 = sem e2 in
      s1 + s2
    | AEminus (e1,e2) ->
      let s1 = sem e1 in
      let s2 = sem e2 in
      if s1 >= s2 
      then (s1 - s2) 
      else negative_natural_number_error ()
    | AEtimes (e1,e2) ->
      let s1 = sem e1 in
      let s2 = sem e2 in
      s1 * s2

(* test *)

let e1 = AEint 3

let e2 = AEplus (AEint 3,AEint 4)

let e3 = AEtimes (e2,AEint 4)

let e4 = AEminus (AEint 3,AEplus (AEint 4,AEint 5))

let e5 = AEminus (AEint 13,AEplus (AEint 4,AEint 5))

let e6 = AEplus (AEint 3,AEtimes (AEint 2,AEint 5))

let e7 = AEtimes (AEplus (AEint 3,AEint 2),AEint 5)

let eval : aexp -> unit =
  fun ae ->
    printfn "%s ==> " (aexp_to_string ae);
    try 
      printfn "%s\n" (eval_to_string (sem ae))
    with 
      Failure message -> 
      printfn "error: %s\n" message

let main =
  List.iter eval [e1;e2;e3;e4;e5;e6;e7]
    
// output 
// 
// 3 ===> 3
// (3 + 4) ===> 7
// ((3 + 4) * 4) ===> 28
// (3 - (4 + 5)) ===> error
// (13 - (4 + 5)) ===> 4
// (3 + (2 * 5)) ===> 13
// ((3 + 2) * 5) ===> 25
// 


