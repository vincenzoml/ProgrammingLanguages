﻿// NOTA: Per ogni esercizio fornire SEMPRE uno o più casi di test!

// Definire una funzione ricorsiva sum, che somma due interi positivi i e j,
// usando esclusivamente l'induzione sui naturali. Utilizzare quindi solo le
// espressioni "1+..." e "n-1" per n > 0.

let rec sum i j = 
    ...

printfn "sum %A:" (sum 3 4)

// Definire la funzione productOf che, dati due interi a,b e una funzione f,
// calcola il prodotto, per i che va da "a" a "b", estremi inclusi, di f(i). Se
// a è maggiore di b, la funzione restituisce 1.

let rec productOf a b f =
    ...

// Scrivere una funzione chiamata multList che, date una lista l e un intero i,
// restituisce la lista in cui ogni elemento di l è moltiplicato per i.

// Usare il costrutto match: Ricordate:
//
// match l with | [] -> expr1 | x::xs -> expr2 
//
// è equivalente a 

// if l = [] 
// then expr1 
// else 
//     let x = head l 
//     let xs = tail l
//     expr2 

// dove in expr2, x e xs sono definiti, e sono rispettivamente la testa e
// la coda di l. 

let rec multList l i = 
    ...


// Definire la funzione couple che, data una lista l di lunghezza pari,
// restituisce una lista di coppie. L'elemento i-esimo del risultato è la coppia
// (x,y) dove x è l'elemento 2i-esimo e y l'elemento (2i+1)-esimo, in altri
// termini gli elementi della lista sono presi a due a due e trasformati in
// coppie. Dunque la lista risultante è lunga la metà. E' un errore passsare una
// lista dispari. Fare un test anche per la condizione di errore.

// NOTA: per catturare l'errore si usa il costrutto "try ... with ... -> ..."
// che permette di catturare un errore senza far andare in crash il runtime (in
// modo da poter continuare l'esecuzione). Si stampa non l'eccezione "e" ma
// "e.Message" che è più conciso.

let rec couple l = 
    ...

printfn "couple: %A" (couple [1;2;3;4])

try 
    printfn "couple: %A" (couple [1;2;3])    
with e -> 
    printfn "Error: %A" e.Message 

// Scrivere la funzione pMul che prende due liste e ne calcola la somma punto a
// punto restituendo una lista della stessa lunghezza di quelle in input, che
// devono essere di pari lunghezza. In altri termini il risultato all'indice i è
// il prodotto degli input all'indice i. Anche in questo caso va segnalato
// errore, nel caso le due liste non siano uguali. Anche in questo caso testare
// anche il caso di errore.

let rec pMul l1 l2 = 
    ...

// Definire la funzione filterBy che data una lista l e una funzione t con
// risultato booleano, restituisce la lista di tutti gli elementi x di l su cui
// t(x) è vero, nello stesso ordine di input

let rec filterBy l t =
    ...

// Definire la funzione fn che data una lista di coppie, una funzione g, e un
// test t, per ogni coppia x,y, calcola l'intero g(x,y) e restituisce poi la
// somma di tutti i g(x,y) ma solo per quelle coppie tali che t(x,y) è true.
// Testarla usando come funzione di test il fatto che uno dei due numeri sia
// divisore dell'altro.

let rec fn l g t =
    ...

// Definire la funzione foldLeft che, data una lista l = [x1;...;xN], una
// funzione di due argomenti f, e un accumulatore/elemento neutro acc,
// restituisce f(f(f(f(acc,x1),x2)...),xN). Sulla lista vuota restituisce acc.
// Per esempio, foldLeft [1;2;3] (fun (x,y) -> x+y) 0 restituisce la somma degli
// elementi. 

let rec foldLeft l f acc =
    ...

printfn "foldLeft: %A" (foldLeft [1;2;3;4] (fun (x,y) -> x*y) 1)