// expressions over _natural_ numbers 

// syntactic domains 

type aexp = 
  AEint of int 
| AEplus of (aexp * aexp) 
| AEminus of (aexp * aexp) 
| AEtimes of (aexp * aexp)

let rec aexp_to_string e =
    match e with
    | AEint i -> sprintf "%d" i
    | AEplus (e1,e2) -> sprintf "(%s + %s)" (aexp_to_string e1) (aexp_to_string e2)
    | AEminus (e1,e2) -> sprintf "(%s - %s)" (aexp_to_string e1) (aexp_to_string e2)
    | AEtimes (e1,e2) -> sprintf "(%s * %s)" (aexp_to_string e1) (aexp_to_string e2)
      
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
      let s1 = sem e1 
      let s2 = sem e2 
      s1 + s2
    | AEminus (e1,e2) ->
      let s1 = sem e1 
      let s2 = sem e2 
      if s1 >= s2 
      then (s1 - s2) 
      else negative_natural_number_error ()
    | AEtimes (e1,e2) ->
      let s1 = sem e1 
      let s2 = sem e2 
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

// Exercise:
// Implement BOOLEAN EXPRESSIONS (and, or, not, and constants)
// FSharp has the type "bool" with values "true" and "false"

// Digression: parallel computation

open System.Threading
open System.Threading.Tasks

let runTasks1 () =

  let rnd = System.Random()

  for i = 1 to 10 do
    let t = new Task(fun () ->
      let time = rnd.Next(3000)
      Thread.Sleep time
      printfn "Thread %d waited for %d milliseconds" i time
    )  
    t.Start()

let seqIncr () = 
  
  let myArray = [| 0 |]

  for i = 1 to 100 do
    Thread.Sleep 100
    myArray[0] <- myArray[0] + 1
  myArray

let seqRes = seqIncr()
printfn "%d" (seqRes[0])

let parIncr1 () = 
  let myArray = [| 0 |]
  for i = 1 to 100 do
    let t = new Task( fun () ->
      Thread.Sleep 100
      myArray[0] <- myArray[0] + 1
      printfn "Thread %d done" i
    )    
    t.Start()
  myArray

let parArr1 = parIncr1()

// Wait until all the "printfn" have been done, before running the next statement!

printfn "%d" (parArr1[0])

let parIncr2 () = 
  let myArray = [| 0 |]
  for i = 1 to 10000 do
    let t = new Task( fun () ->
      myArray[0] <- myArray[0] + 1
      printfn "Thread %d done" i
    )    
    t.Start()
  myArray

let parArr2 = parIncr2()

// Wait until all the "printfn" have been done, before running the next statement!

printfn "%d" (parArr2[0])

let parIncr3 () = 
  let rnd = System.Random()
  let myArray = [| 0 |]
  for i = 1 to 10000 do
    let t = new Task( fun () ->
      let x = myArray[0]  // Read the value of the element
      Thread.Sleep (rnd.Next(10))  // Sleep for a random amount of time (up to 10ms)
      myArray[0] <- x + 1 // increase the element
      printfn "Thread %d done" i
    )    
    t.Start()
  myArray

let parArr3 = parIncr3()

// Wait until all the "printfn" have been done, before running the next statement!

printfn "%d" (parArr3[0])

// NOTE: we DO have "wait" instructions in fsharp, but it's even easier to do:


let parIncr4 () =
  let rnd = System.Random()
  let myArray = [| 0 |]

  Array.Parallel.iter (fun i ->

    let x = myArray[0]
    Thread.Sleep (rnd.Next 10)
    myArray[0] <- x + 1
    printfn "Thread %d done" i

  ) [| 1 .. 1000 |]

  myArray[0]

let parRes4 = parIncr4()

// No need to wait anymore!

printfn "%d" parRes4


let parIncr5 () =
  let rnd = System.Random()
  let myArray = [| 0 |]

  Array.Parallel.iter (fun i ->

    lock myArray (fun () -> 
      let x = myArray[0]
      Thread.Sleep (rnd.Next 10)
      myArray[0] <- x + 1
      printfn "Thread %d done" i
    )
  ) [| 1 .. 1000 |]

  myArray[0]

printfn "%d" (parIncr5 ())

let parIncr6 () =
  let rnd = System.Random()
  let myArray = [| 0 |]

  Array.Parallel.iter (fun i ->

    lock myArray (fun () -> 
      let x = myArray[0]
      Thread.Sleep (rnd.Next 10)    
      myArray[0] <- x + 1
      printfn "Thread %d done" i
    )
  ) [| 1 .. 1000 |]

  myArray[0]

printfn "%d" (parIncr5 ())

// Is lock all that we need?

// Dining philosophers

open System.Threading
open System.Threading.Tasks

let think =
  let rnd = System.Random ()
  fun () -> 
    Thread.Sleep (rnd.Next 100)

let eat =
  let rnd = System.Random ()
  fun () -> 
    Thread.Sleep (rnd.Next 100)

let philosopher numPhilosophers (chopsticks : array<array<unit>>) id =
  let rnd = System.Random()
  for i = 1 to 10 do
    let choice = rnd.Next 2 // flip a coin
    let firstChop = (id + choice) % numPhilosophers
    let sndChop = (id + (1-choice)) % numPhilosophers
    think ()
    lock chopsticks[firstChop] (fun () ->
      printfn "Philosopher %d acquired chopstick %d (1)" id firstChop      
      think ()
      lock chopsticks[sndChop] (fun () ->
        printfn "Philosopher %d acquired chopstick %d (2)" id sndChop        
        printfn "Philosopher %d is eating (round %d)" id i
        eat ()
        printfn "Philosopher %d finished eating (round %d)" id i
      )
    )
  printfn "*** Philosopher %d going home" id


let runPhilosophers numPhilosophers =
  let chopsticks = Array.init numPhilosophers (fun i -> [|()|])
  Array.Parallel.iter (fun i -> philosopher numPhilosophers chopsticks i) [|0..numPhilosophers-1|] 

runPhilosophers 3
