// Scritto 21/06/2023

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
// Definire la funzione "rectangleMethod" con tipo (float -> float) -> float ->
// float -> int -> float
//
// Data una funzione "f" da float a float, l'estremo inferiore "a", l'estremo
// superiore "b", e il numero "n" di intervalli, questa funzione calcola
// l'integrale definito di "f" fra "a" e "b" usando il metodo dei rettangoli,
// con "n" suddivisioni. 
//
// Vale a dire: 
//
// 1. definire il delta "dx" come (b-a)/n
//
// 2. calcolare la somma, per i che va da 1 ad n, di dx*f(x_i), dove x_i è il
//    punto medio dell'intervallo [a+dx*(i-1),a+dx*i].
//
// Se necessario, per convertire un intero in un float, usare la funzione predefinita "float".ù
// Per calcolare la somma di cui al punto 2, usare una funzione ricorsiva ausiliaria.


let rectangleMethod (f: float -> float) (a: float) (b: float) (n: int) =
    ...



// TEST

let mySin = fun x -> sin (x*System.Math.PI)

try 
    printfn "%A" (rectangleMethod id 0 1 1)  // 0.5
    printfn "%A" (rectangleMethod id 0 1 10)
    printfn "%A" (rectangleMethod (fun x -> 1) 0 1 1) // 1.0
    printfn "%A" (rectangleMethod (fun x -> 1) 0 1 10)
    printfn "%A" (rectangleMethod mySin 0 1 1) // 1.0
    printfn "%A" (rectangleMethod mySin 0 1 2) // 0.70...
    printfn "%A" (rectangleMethod mySin 0 1 10) // 0.63...
with 
    err -> printfn "Error: %A" err


// ESERCIZIO 2

// Definire la funzione ricorsiva not_useful che data una lista l di elementi di
// un qualunque tipo 'a, e una funzione f : 'a -> int, restituisce la lista di
// coppie (f(x)-10,f(x)+2) per ogni elemento x, nell'ordine in cui sono presenti
// in l, ma solo per gli elementi in cui il primo numero è maggiore di 0, e il
// secondo è minore di 200

let rec not_useful l f =
    ...


// TEST:

printfn "%A" (not_useful [1;2;10;9;13;20] (fun x -> x * x))
// ANCORA UN TEST

// ESERCIZIO 3

// Definire un tipo dato enumerato in FSharp chiamato FN che possa rappresentare
// alcune funzioni elementari a scelta (es: seno, coseno, somma di una
// costante). Definire una funzione ricorsiva che, dato un valore x: float e una
// lista di FN, l= [fn1,...,fnN], calcola il valore di fnN(...(fn2(fn1(x))))

type FN = SIN | COS | ADD of float | ...  // CAMBIARE A PIACERE!

let rec evaluateFN x (l : list<FN>) = 
    ...

// TEST



// Esercizio 4

// Completare l'interprete proposto nel seguito, utilizzando la regola dello
// SCOPING STATICO, definendo il comando CStrangeUntil of pseq * ide * ide * exp
// che, data una sequenza di comandi ps, tre identificatori i1 e i2, una
// espressione e, esegue ps almeno una volta, e continua a ripetere l'esecuzione
// finchè i valori delle variabili denotate da i1 e i2 non sono uguali. In più,
// alla fine di ogni ciclo (incluso il primo), se l'espressione e denota il
// valore di verità "true", nell'AMBIENTE che esiste dopo la valutazione di ps
// (quindi, e è moralmente nella stessa sequenza di comandi ps, con lo stesso
// scoping, e le variabili locali di ps sono visibili in e) allora i VALORI
// delle variabili i1 e i2 vengono scambiati.
//

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
    | CStrangeUntil of pseq * ide * ide * exp
    
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
    | CStrangeUntil (ps,i1,i2,e) -> sprintf "until %s and %s become equal do\n%s\nswap %s and %s when %s" i1 i2 (pseq_to_string ps) i1 i2 (exp_to_string e) 

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
        | CStrangeUntil (ps,i1,i2,e) ->
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
    [  
        Prog ... 
    ]

let _ = List.iter evaluate l
