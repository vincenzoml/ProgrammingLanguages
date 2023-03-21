// FSharp: functions, recursion, abstract data types, & c.

// This file is not meant to be run directly with "dotnet run" since it also contains errors
// Evaluate it line by line using the interpreter, or comment all the lines and then
// uncomment just one line and use dotnet run

// Comments are like this one


/// Printf in all flavours

printfn "Hello world" (* Hello world -- Yes, one can also comment in this way *)

printfn "%d" 3 
printfn "%s" "hello"
printfn "The result of %d + %d is %d" 3 2 5
printfn "%A %A %A" 3.0 3 "3"

let f (x,y) =
    x+y

let f x y = 
    x + y

/// Simple expressions

printfn "three plus three is %d" (3+3)
// Error:
// printfn "%d" 3+3
// because it's parsed as (printfn "%A")(3)(+3)

/// Let binding

let x1 = 3

printfn "%A" x1

let plus (x,y) =  x + y


// Let's try a more complicated example

let ratio(x,y) =
    let z = x * y
    let w = (2*x) + (2*y)
    w / z

printfn "%A" (ratio(1,225))
printfn "%A" (ratio(15,15))

// Let us get to types!
let getType(x) = x.GetType().FullName

printfn "%A" (getType("3"))

// Question: what's this? Before running it!

printfn "%A" (getType(getType("3")))

// Note the difference with

printfn "%A" ( "3".GetType().GetType().FullName )

// Back to the original problem in the definition of g

printfn "%A" (getType 3.0)  // Note: I'm starting to omit unnecessary parentheses
printfn "%A" (getType 3)

// Try to evaluate in the interpreter
// (3 : int)
// (3 : double)

// Try to evaluate
// (3.0 : int)

// Note the ":" construct. It does not change the semantics, it only checks it from the point of view of the user.
let ratio2(x : float,y : float) =
    let z = x * y
    let w = (2.0 * x) + (2.0 * y)
    (w / z  : float)

printfn "%A" (ratio2(1,225))
printfn "%A" (ratio2(15,15))

// Conditional EXPRESSIONS (the equivalent of "condition ? e1 : e2" in languages derived from C)

let x = 
    if 5 > 4 then
        (failwith "STUB")
    else
        "OK"

;;


// Recursion

let rec recFn x = // Note that I'm also omitting parentheses here
    if x <= 0 then 
        0 // Note: return is NOT needed in functions
    else 
        2 + (recFn (x - 1)) // why do I need parentheses around (x-1) ?

printfn "%d" (recFn 7)

// Anonymous functions (lambda-calculus-like)

printfn "%d" ((fun x -> x + 1)(10))

// Anonymous functions can't be recursive!

// Tuples vs functional abstraction

// Recall the definition of plus
// let plus(x,y) = x + y

let plus' x y = x + y

// Same as:
// let plus' = (fun x -> (fun y -> (x + y)))

let fn1 = plus' 3

// Q: when you write plus (2,3), how many arguments does f1 get? Of which type?

let pair = (2,3)

// Currying and uncurrying

let curry = fun f (x,y) -> f x y
let uncurry = fun f -> fun x -> fun y -> f (x,y)

let fn2 = curry plus'

let plus'' (x,y) = plus' x y

// Tuples

let triplet = (6,1,32)

let quadruple = (3,4,23,12)

// Q: what is the following value?

let quadruple' = (3,(4,23),12)

printfn "%A" (fst (1,2))
printfn "%A" (snd (1,2))

// Functional programming

let functional1 (f : int -> int) x = 1 + (f x)

// Q: what does functional2 do in the following?
let functional2 (f : int -> int) x = (functional1 f x) - 1

let double x = x * 2

let succ x = x + 1

printfn "%A" (functional2 succ 3)

let functional3 = functional2 succ

printfn "%A" (functional3 3)

// Conditional expressions

let val1 = 3
let cond1 = if val1 > 4 then 1 else 0

// Note: boolean expressions are also expressions:

let bool1 = val1 > 4
let cond2 = if bool1 then 1 else 0

// Recursion!

let rec recFn1 x = 
    if x <= 0 then 1 else 1 + (recFn1 (x-1))

// Try to remove rec see what's the error. Why?

let rec recFn2 x = 
    if x <= 0 then 0        
    else 2 + (recFn2 (x - 1))

// Aha! Let's see:
// let rec recFn3 x = 
//     x + (recFn3 x)

/////// 
/// Exercises: define the following functions, each time test them on a few cases.

/// Fibonacci

/// Sum of the first k numbers

let rec sum k = 
    if k <= 0 then 0 else k + (sum (k-1))

/// Product of the first k numbers

/// Sum for i in [1,k] of f(i), defined using a function, taking as arguments k and f

let rec sum2 k f = 
    if k <= 0 then 0 
    else (f k) + (sum2 (k-1) f) 

/////////
/// 