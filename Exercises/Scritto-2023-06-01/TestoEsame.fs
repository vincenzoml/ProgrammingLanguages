// Scritto 01/06/2023

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


////////////////////////////////////////////////////////////////////////////
// PARTE 1: DA SALTARE E CANCELLARE PER CHI HA FATTO IL COMPITINO

// PARTE 1 ESERCIZIO 1 
//
// Definire la funzione ricorsiva pow, che dati due numeri naturali a e b, calcola a elevato alla potenza b, utilizzando solo il prodotto.

let rec pow a b = 
    ... // COMPLETARE

// TEST
try 
    printfn "pow(%A,%A)=%A" 5 2 (pow 5 2)
    printfn "pow(%A,%A)=%A" 0 1 (pow 0 1)
    printfn "pow(%A,%A)=%A" 3 0 (pow 3 0)
    printfn "pow(%A,%A)=%A" 1 3 (pow 1 3)
    printfn "pow(%A,%A)=%A" -5 2 (pow -5 2)
    printfn "pow(%A,%A)=%A" 5 -2 (pow 5 -2)
with 
    err -> printfn "Error: %A" err


// PARTE 1 ESERCIZIO 2

// Definire la funzione findMinOfFunctionWhen che, data una lista l : List<int>,
// una funzione f : int -> float, e un predicato di filtro flt : int -> bool,
// calcola il minimo degli f(i), per i in l, solo per gli elementi in cui flt(i)
// restituisce true (sull'indice). La funzione restituisce infinity (che è un
// valore di tipo float in fsharp) se nessun elemento soddisfa il predicato di
// filtro. Usare la funzione predefinita fsharp "min" per trovare il minimo fra
// due elementi. NB: Per convertire un int in float usare la funzione "float" :
// int -> float.

let rec findMinOfFunctionWhenRec l f flt i acc = // Questo è un suggerimento, ma siete liberi di implementare diversamente
    ... // COMPLETARE

let findMinOfFunctionWhen l f flt = 
    ... // COMPLETARE

// TESTARE!!!

...

// PARTE 1 ESERCIZIO 3

// Definire un tipo dato in FSharp che possa rappresentare una lista di comandi
// per far muovere un robot in uno spazio UNIDIMENSIONALE (la retta dei reali).
// NON si può usare il tipo di dato predefinito "List" di FSharp. Le mosse sono
// "Left" (andare verso -infinity) e "Right" (andare verso +infinity), e ogni
// mossa contiene, in aggiunta, la distanza (di tipo float) da percorrere.
// Scrivere una funzione ricorsiva che, data una lista di mosse, e una posizione
// iniziale (di tipo float), calcola la posizione finale del robot dopo aver
// eseguito tutti i passi con successo. 

type Moves = ... // COMPLETARE

let rec finalPos moves initialPos =
    ... // COMPLETARE

// TEST: 

... // COMPLETARE

////////////////////////////////////////////////////////////////////////////
// PARTE 2 - DA FARE TUTTI
// 

// Si completino le definizioni date nel seguito per definire l'interprete di un
// linguaggio di programmazione imperativo con scoping statico, in cui occorre
// in particolare implementare le seguenti specifiche:

// PARTE 2 Esercizio 1

// Definire il comando di assegnamento condizionale "CondAssign of ide * expr *
// expr". La semantica intesa è che "CondAssign (i,e1,e2)" assegna alla
// variabile denotata da "i" il valore dell'espressione "e1" SOLO SE "e2" è
// vera.  

// PARTE 2 Esercizio 2

// Definire il comando "CFixPoint of ide * pseq" dove "CFixPoint (i,ps)" esegue
// la sequenza di comandi ps fino a quando il valore della variabile denotata
// dall'identificatore "i" non rimane invariato dopo una esecuzione. La sequenza
// di comandi deve sempre essere eseguita almeno una volta.

// NOTA BENE:

// Fornire un programma (tipo "Prog") che mostri insieme i due costrutti facendo
// fare almeno una iterazione a CFixPoint, oltre quella iniziale. Se si
// implementa solo uno dei due esercizi, fornire comunque un programma di test.

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
    | CCondAssign of ide * exp * exp
    | CFixPoint of ide * pseq
    
and pseq =
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
    | CCondAssign (i,e1,e2) -> sprintf "%s := %s when %s" i (exp_to_string e1) (exp_to_string e2)
    | CFixPoint (i,ps) -> sprintf "fix %s in\n%s" i (pseq_to_string ps)

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
        | CCondAssign (i,e1,e2) ->
            ... // COMPLETARE
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
        | CFixPoint (i,ps) ->
            ... // COMPLETARE
        
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
    [ Prog(
        ... // COMPLETARE
      ) ]

let _ = List.iter evaluate l
