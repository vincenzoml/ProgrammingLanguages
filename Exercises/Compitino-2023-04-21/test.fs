// Compitino 21/04/2023

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
// Definire la funzione ricorsiva fact, che calcola il fattoriale di un intero;
// la funzione non è definita sugli interi minori di 1 (usare failwith)

let rec fact i = 
    ...
// TEST
printfn "fact(%A)=%A" 5 (fact 5)

try 
    printfn "fact(%A)=%A" 0 (fact 0)
with 
    msg -> printfn "Error: %A" msg

// ESERCIZIO 2
//
// Definire la funzione sumIfEven che, dati due interi a,b e una funzione f,
// calcola la somma, per i che va da "a" a "b", estremi inclusi, di f(i), solo
// sugli indici i tali che f(i) è pari (non "i", ma "f(i)"). Quando l'intervallo
// [a,b] è vuoto, la somma è 0. NB: l'operatore di modulo è "%"
//
// Se a è maggiore di b, la funzione restituisce 1.

let rec sumIfEven a b f =
    ...

let inc x = x + 1

printfn "sumIfEven 10 10 inc = %A" (sumIfEven 10 10 inc) 
printfn "sumIfEven 11 11 inc = %A" (sumIfEven 10 10 inc) 
printfn "sumIfEven 10 15 inc = %A" (sumIfEven 10 15 inc) 

// Aggiungere un test con una funzione diversa da inc

// Da ora in poi, quando possibile, usare il costrutto match.

// NOTA BENE: 
//
// match l with 
// | [] -> expr1 
// | x::xs -> expr2 
//
// è equivalente a 

// if l = [] 
// then expr1 
// else 
//     let x = head l 
//     let xs = tail l
//     expr2 

// Pertanto, in expr2, x e xs sono definiti, e sono rispettivamente la testa e
// la coda di l.

// ESERCIZIO 3

// Definire la funzione findMaxBy che, data una lista l : List<float>, una
// funzione f : float -> float, e una funzione di filtro filter : float -> bool,
// calcola il massimo degli f(i), per i in l, solo per gli elementi su cui
// filter restituisce true. La funzione restituisce "-infinity" (che è un valore
// in F#) se nessun elemento soddisfa il predicato di filtro.
// Definire la funzione ausiliaria "max" per calcolare il massimo fra due elementi

let max x y = ...

let rec findMaxBy l f filter =
    ...

// TEST


// ESERCIZIO 4
// 
// Definire le funzioni p1List e p2List, che data una lista di coppie,
// restituiscono rispettivamente la lista degli elementi sinistri (destri).

let rec p1List l =
    ...

let rec p2List l =
    ...
    
// TEST

// ESERCIZIO 5
//
// Definire il prodotto scalare di due vettori, rappresentati come due liste di uguale lunghezza. La funzione non è definita quando i vettori hanno dimensione diversa 

let rec scalar l1 l2 =
    ...

// TEST

// Definire la funzione double, che data una lista l, restituisce la lista ll ottenuta concatenando l con se stessa. Definire le funzioni ausiliarie ricorsive necessarie.

let double l =
    ...

// TEST

// ESERCIZIO 6
// 
// Dato il tipo 

type Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

// Definire la funzione "findWeekend" che, data una lista l : List<Weekday>, restituisce true se la lista contiene la sequenza "Sat; Sun", e false altrimenti

let rec findWeekend l =
    ...

// TEST

// ESERCIZIO 7

// Dato il tipo 

type Num = Int of int | Float of float

// Definire la funzione sumNum che, dati due elementi del tipo Num, restituisce
// la loro somma. In particolare, se la somma è garantita essere un Int, perchè
// entrambi gli elementi hanno etichetta Int, allora viene restituito un Int,
// altrimenti sempre un Float Per convertire un int in float si può usare la
// funzione fsharp "float", es: "float 3" restituisce 3.0.

let sum x y =
    ...
    
// TEST


// ESERCIZIO 8 OPZIONALE
// Dato il tipo

type Bintree = Leaf | Node of float * Bintree * Bintree

// definire la funzione ricorsiva maxBinTree che trova il massimo elemento
// dell'albero, restituendo -infinity su un albero vuoto (Leaf)

let rec maxBinTree t =
    ...

printfn "maxBinTree: %A" (maxBinTree (Node (3.0,Leaf,Node (5.0,Leaf,Leaf))))

// INSERIRE UN ALTRO TEST