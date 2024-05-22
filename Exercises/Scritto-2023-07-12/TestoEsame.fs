// Scritto 12/07/2023

// Per ogni funzione richiesta, mostrare 2 casi di test. 

// IMPORTANTE: quando si testano funzioni che possono andare in crash, usare il
// costrutto try ... with per evitare che l'interprete esca completamente, e
// invece stampare il messaggio di errore, come nel seguente 

// ESEMPIO DI GESTIONE DELLE ECCEZIONI

let f x = 
    if x <= 0 
    then failwith "questa funzione è definita solo su interi positivi"
    else x - 1

try
    printfn "%A" (f (-1)) // TESTO LA MIA FUNZIONE
with msg -> // quando c'è un errore, questo viene associato al nome "msg" che può essere usato nel blocco sotto 
    printfn "%A" msg  


// ESERCIZIO 1 
//
// Definire la funzione products che, data una lista l di interi, restituisce
// una lista della stessa lunghezza in cui, nella posizione i-esima, è
// memorizzato il prodotto dei primi i elementi della lista di input

let rec productsAux l accum =
    ...

let products l = ... 

// TEST
try 
    printfn "%A" (products [])
    printfn "%A" (products [17])
    printfn "%A" (products [2;3;5])   
with 
    err -> printfn "Error: %A" err


// ESERCIZIO 2

// Definire la funzione computeAverageBy che, data una lista lst di valori di
// tipo float, una funzione di filtro flt : float -> bool e una funzione fn :
// float -> float, calcola la media dei valori di fn(i), per i elemento della
// lista, considerando però solo i valori su cui la funzione flt restituisce
// true. Restituire 0 se non c'è almeno un valore su cui calcolare la media. 

// Per forzare un parametro ad essere di tipo float utilizzare la notazione (n:
// float) nel passaggio di parametri


let computeAverageBy lst flt fn = ...


// ESERCIZIO 3

// Definire un tipo di dato enumerato in FSharp che rappresenti figure
// geometriche, con le lunghezze dei lati (Ad esempio: TRIANGOLO(3.0,5.0,2.0),
// QUADRATO(...). Non importa garantire che la figura sia effettivamente
// realizzabile (ad esempio non è necessario controllare che il triangolo
// dell'esempio non sia degenere). E' sufficiente avere diverse figure
// geometriche con una t-upla di lunghezze. Scrivere una funzione ricorsiva che
// data una lista di queste figure, calcoli la somma dei perimetri delle figure
// stesse.

type Figures = ...

let totalPerimeter = ...


// Esercizio 4

// Dato il tipo enumerato FSharp che rappresenta mosse sul piano cartesiano
// (andare in alto, in basso, a destra, a sinistra, o teletrasportarsi)

type Move = Up of float | Down of float | Left of float | Right of float | Teleport of (float * float)

// scrivere una funzione ricorsiva che, dato un punto iniziale in R^2 (nella
// forma di una coppia (x0: float, y0:float)), calcola la posizione di arrivo.

let rec computeFinalPoint (x0 : float, y0 : float) lst = ...


// Completare l'interprete proposto nel seguito, utilizzando la regola dello
// SCOPING STATICO, definendo il comando CSumUntil of (ide * ide * exp * pseq)
// che, dati due identificatori x e y, e una sequenza di comandi ps, se expr
// valuta al booleano True, somma alla variabile denotata da x il valore della
// variabile denotata da y, poi esegue ps, e ripete il procedimento. Se expr
// valuta al booleano False, il comando non fa nulla.

// Fornire un caso di test

(* syntax *)

type ide = string

type exp =
    | Eint of int
    | Ebool of bool // NB: è il tipo dato "booleani di macchina", ovvero il tipo dato nativo in fsharp.
    | Eide of ide
    | Eplus of (exp * exp)    
    | Eeql of (exp * exp)

type com =
    | Cassign of ide * exp
    | Cvar of ide * exp
    | Cconst of ide * exp
    | CSumUntil of (ide * ide * exp * pseq)
    
and pseq = // Equivalente a List<com>
    | Pseq of com * pseq
    | Pend

type prog = Prog of pseq * exp

let rec exp_to_string (e: exp) =
    match e with
    | Eint i -> sprintf "%d" i
    | Eplus (e1, e2) -> sprintf "(%s + %s)" (exp_to_string e1) (exp_to_string e2)
    | Eide i -> i
    | Ebool b -> sprintf "%A" b        
    | Eeql (e1, e2) -> sprintf "(%s == %s)" (exp_to_string e1) (exp_to_string e2)

let rec com_to_string (c: com) =
    match c with
    | Cassign (i, e) -> sprintf "%s := %s" i (exp_to_string e)
    | Cvar (v, e) -> sprintf "var %s := %s" v (exp_to_string e)
    | Cconst (v, e) -> sprintf "const %s = %s" v (exp_to_string e)
    | CSumUntil (i1,i2,e,ps) -> sprintf "while %s sum %s to %s and do\n%s\n" i1 i2 (exp_to_string e) (pseq_to_string ps) 

and pseq_to_string (s: pseq) =
    match s with
    | Pseq (c, Pend) -> sprintf "%s" (com_to_string c)
    | Pseq (c, q) -> sprintf "%s;\n%s" (com_to_string c) (pseq_to_string q)
    | Pend -> ""

let prog_to_string (p: prog) =
    match p with
    | Prog (s, e) -> sprintf "%s;\nreturn %s" (pseq_to_string s) (exp_to_string e)

(* error handling *)

let unbound_identifier_error ide =
    failwith (sprintf "unbound identifier %s" ide)

let negative_natural_number_error () =
    failwith "natural numbers must be positive or zero"

let type_error () = failwith "type error"

let memory_error () =
    failwith "access to a location that is not available"

let not_a_location_error i =
    failwith (sprintf "not a location: %s" i)

(* semantic domains *)

type eval = Int of int | Bool of bool 
    
type loc = int

type mval = eval

type store = int * (loc -> mval) (* il primo elemento della coppia è la minima locazione non definita *)

let empty_store = (0, (fun l -> memory_error ()))

let apply_store st l = (snd st) l

let allocate: store -> loc * store =
    fun st ->
        let l = fst st in
        let l1 = l + 1 in
        let st1 = (l1, snd st) in
        (l, st1)

let update: store -> loc -> mval -> store =
    fun st l mv ->
        match st with
        | (maxloc, fn) -> let fn1 l1 = if l = l1 then mv else fn l1 in (maxloc, fn1)

type dval =
    | E of eval
    | L of loc

type env = ide -> dval

let empty_env = fun v -> unbound_identifier_error v

let bind e v r = fun v1 -> if v1 = v then r else e v1

let apply_env e v = e v

let rec eval_to_string (e: eval) =
    sprintf "%A" e


(* denotational semantics *)

let rec esem: exp -> env -> store -> eval =
    fun e ev st ->
        match e with
        | Eint i ->
            if i < 0 then
                negative_natural_number_error ()
            else
                Int i // COMPLETARE
        | Eplus (e1, e2) ->
            let s1 = esem e1 ev st
            let s2 = esem e2 ev st
            match (s1,s2) with
            | Int i1, Int i2 -> Int (i1+i2) 
            | _ -> type_error()        
        | Ebool b ->
            Bool b 
        | Eeql (e1, e2) ->
            let s1 = esem e1 ev st in
            let s2 = esem e2 ev st in
            match (s1, s2) with
            | (Int i1, Int i2) -> Bool (i1=i2)
            | _ -> type_error ()
        | Eide i ->
            let value = apply_env ev i
            match value with
            | L l -> apply_store st l
            | E e -> e

let rec csem: com -> env -> store -> (env * store) =
    fun c ev st ->
        match c with
        | Cassign (i, e) ->
            let s = esem e ev st
            match apply_env ev i with
            | L l -> let st1 = update st l s in (ev, st1)
            | _ -> not_a_location_error i
        | Cvar (i, e) ->
            let s = esem e ev st in
            let (newloc, st1) = allocate st in
            let st2 = update st1 newloc s in
            let ev1 = bind ev i (L newloc) in
            (ev1, st2)
        | Cconst (i, e) ->
            let s = esem e ev st in
            let ev1 = bind ev i (E s) in
            (ev1, st)
        | CSumUntil (i1,i2,e,ps) ->
            ...    
            
        
        
and pssem: pseq -> env -> store -> (env * store) =
    fun s ev st ->
        match s with
        | Pend -> (ev, st)
        | Pseq (c, q) ->
            match csem c ev st with
            | (ev1, st1) -> pssem q ev1 st1

let rec psem: prog -> env -> store -> eval =
    fun p ev st ->
        match p with
        | Prog (s, e) ->
            let (ev1, st1) = pssem s ev st
            esem e ev1 st1

(* test *)

let evaluate: prog -> unit =
    fun p ->
        printf "\n%s \n\n==> " (prog_to_string p)

        try            
            printf "%s\n" (eval_to_string (psem p empty_env empty_store))
        with
        | Failure message -> printfn "error: %s\n" message

let l =
    ...
    
let _ = List.iter evaluate l
