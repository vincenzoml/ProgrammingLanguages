(* syntax *)

type ide = string
  
type boolean = True | False
    
type exp = 
| Eint of int 
| Eplus of (exp * exp) 
| Eminus of (exp * exp) 
| Eide of ide 
| Ebool of boolean
| Eeql of (exp * exp)
| Enot of exp
| Eand of (exp * exp)
| Eor of (exp * exp) 
| Eifthenelse of (exp * exp * exp)

type com = 
| Cassign of ide * exp 
| Cvar of ide * exp 
| Cconst of ide * exp

type prog = 
| Pseq of com * prog 
| Pend of exp

let rec exp_to_string (e : exp) =
  match e with
    Eint i -> sprintf "%d" i
  | Eplus (e1,e2) -> sprintf "(%s + %s)" (exp_to_string e1) (exp_to_string e2)
  | Eminus (e1,e2) -> sprintf "(%s - %s)" (exp_to_string e1) (exp_to_string e2)
  | Eide i -> i
  | Ebool b -> 
    (match b with
      True -> "true"
    | False -> "false")
  | Enot e -> Printf.sprintf "(not %s)" (exp_to_string e)
  | Eand (e1,e2) -> Printf.sprintf "(%s and %s)" (exp_to_string e1) (exp_to_string e2)
  | Eor (e1,e2) -> Printf.sprintf "(%s or %s)" (exp_to_string e1) (exp_to_string e2)
  | Eeql (e1,e2) -> Printf.sprintf "(%s == %s)" (exp_to_string e1) (exp_to_string e2)
  | Eifthenelse (c,e1,e2) -> Printf.sprintf "if %s then (%s) else (%s)" (exp_to_string c) (exp_to_string e1) (exp_to_string e2)

let rec com_to_string (c : com) =
  match c with
  | Cassign (i,e) -> 
      sprintf "%s := %s" i (exp_to_string e)
  | Cvar (v,e) -> 
      sprintf "var %s := %s" v (exp_to_string e)
  | Cconst (v,e) -> 
      sprintf "const %s = %s" v (exp_to_string e)

let rec prog_to_string (p : prog) = 
  match p with
  | Pseq (c,q) -> Printf.sprintf "%s;\n%s" (com_to_string c) (prog_to_string q)
  | Pend e -> sprintf "return %s" (exp_to_string e)

(* error handling *)

let unbound_identifier_error ide = 
  failwith (Printf.sprintf "unbound identifier %s" ide)

let negative_natural_number_error () = failwith "natural numbers must be positive or zero"

let type_error () = failwith "type error"

let memory_error () = failwith "access to a location that is not available"

let not_a_location_error i = failwith (Printf.sprintf "not a location: %s" i)

(* semantic domains *)
type eval = Int of int | Bool of bool 

type loc = int

type mval = eval // I valori memorizzabili sono uguali a quelli esprimibili

type store = int * (loc -> mval) (* il primo elemento della coppia è la minima locazione non definita *)

let empty_store = (0,fun l -> memory_error ())

let apply_store (maxloc,fn) l = fn l

let allocate : store -> loc * store = 
  fun (maxloc,fn) ->     
    let newMaxLoc = maxloc + 1 in
    (maxloc,(newMaxLoc, fn))
    
let update : store -> loc -> mval -> store = 
  fun st l mv ->
    match st with
      (maxloc,fn) ->
        if l >= maxloc then memory_error() 
        else
          let fn1 l1 = 
            if l = l1 
            then mv 
            else fn l1 
          (maxloc,fn1)

type dval = E of eval | L of loc // I valori denotabili contengono le locazioni e sono dunque diversi da quelli esprimibili 

type env = ide -> dval 

let empty_env = fun v -> unbound_identifier_error v
  
let bind e v r = 
  fun v1 -> 
    if v1 = v
    then r
    else e v1
      
let apply_env e v = e v

let rec eval_to_string (e : eval) =
  match e with
    Int i -> Printf.sprintf "%d" i
  | Bool b -> if b then "true" else "false"

(* denotational semantics *)
  
let rec esem : exp -> env -> store -> eval = fun e ev st ->
      match e with
        Eint i -> 
          if i < 0 
          then negative_natural_number_error ()
          else Int i
      | Eplus (e1,e2) -> 
        (let s1 = esem e1 ev st in
         let s2 = esem e2 ev st in
         match (s1,s2) with
           (Int i1,Int i2) -> Int (i1 + i2)
         | _ -> type_error ())
      | Eminus (e1,e2) ->
        let s1 = esem e1 ev st in
        let s2 = esem e2 ev st in
        (match (s1,s2) with
          (Int i1,Int i2) ->
            if i1 >= i2 
            then Int (i1 - i2) 
            else negative_natural_number_error ()
        | _ -> type_error ())
      | Ebool b -> 
        (match b with 
          True -> Bool true 
        | False -> Bool false)
      | Eeql (e1,e2) ->
        let s1 = esem e1 ev st in
        let s2 = esem e2 ev st in
        (match (s1,s2) with
          (Int i1,Int i2) ->
            if i1 = i2
            then Bool true
            else Bool false
        | _ -> type_error ())
      | Eand (e1,e2) ->
        let s1 = esem e1 ev st in
        let s2 = esem e2 ev st in 
        (match (s1,s2) with
          (Bool b1,Bool b2) -> Bool (b1 && b2)
        | _ -> type_error ())
      | Eor (e1,e2) ->
        let s1 = esem e1 ev st in
        let s2 = esem e2 ev st in 
        (match (s1,s2) with
          (Bool b1,Bool b2) -> Bool (b1 || b2)
        | _ -> type_error ())
      | Enot e ->
        let s = esem e ev st in
        (match s with
          Bool b -> Bool (not b)
        | _ -> type_error ())
      | Eifthenelse (c,e1,e2) ->
        let sc = esem c ev st in
        (match sc with
          Bool b -> 
            if b 
            then esem e1 ev st
            else esem e2 ev st            
        | _ -> type_error ())
      | Eide i ->
          let value = apply_env ev i
          match value with
          | L l -> apply_store st l 
          | E e -> e

let csem : com -> env -> store -> (env * store) = 
  fun c ev st ->
    match c with
    | Cassign (i,e) ->
        match apply_env ev i with
        | L l ->
            let s = esem e ev st        
            let st1 = update st l s in
            (ev,st1)
        | _ -> not_a_location_error i
    | Cvar (i,e) ->
        let s = esem e ev st in
        let (newloc,st1) = allocate st in
        let st2 = update st1 newloc s in
        let ev1 = bind ev i (L newloc) in
        (ev1,st2)
    | Cconst (i,e) ->
      let s = esem e ev st in
      let ev1 = bind ev i (E s) in
      (ev1,st)
        
let rec psem : prog -> env -> store -> eval = 
  fun p ev st ->
    match p with
      Pend e -> esem e ev st 
    | Pseq (c,q) -> 
      match csem c ev st with
        (ev1,st1) -> psem q ev1 st1

(* test *)
let eval : prog -> unit =
  fun p ->
    Printf.printf "\n%s \n\n==> " (prog_to_string p);
    try 
      Printf.printf "%s\n" (eval_to_string (psem p empty_env empty_store))
    with 
      Failure message -> 
        Printf.printf "error: %s\n" message


let l = 
  [ Pseq (Cconst ("x",Eint 5), 
      Pseq (Cvar ("y",Eint 0), 
        Pseq (Cassign ("y",Eide "x"),
          Pseq (Cassign ("y",Eplus (Eide "y",Eint 1)),
                  Pend (Eplus (Eide "x",Eide "y")))))); 

    Pseq (Cconst ("x",Eint 5), 
      Pseq (Cvar ("y",Eint 0), 
        Pseq (Cassign ("y",Eide "x"),
          Pseq (Cassign ("x",Eplus (Eide "y",Eint 1)),
                  Pend (Eplus (Eide "x",Eide "y")))))); 
  ]
  
let main =
  List.iter eval l



// ESERCIZIO:
// Implementare il *comando* ifthenelse (diverso dall'espressione condizionale), vale a dire aggiungere a "type com" il caso "CIfThenElse of exp * com * com", con la seguente semantica intesa: se l'espressione passata come primo argomento valuta a true, si deve eseguire solo il primo comando (ramo then), mentre in caso contrario si deve eseguire solo il ramo else. Creare un programma di esempio che dimostri che gli assegnamenti a variabili (unico tipo di side effect imperativo del nostro linguaggio) siano eseguiti in un ramo then o else solo se quel ramo viene valutato.