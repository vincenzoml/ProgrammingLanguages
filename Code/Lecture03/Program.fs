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

/// Simple expressions

printfn "three plus three is %d" (3+3)
// Error:
// printfn "%d" 3+3
// because it's parsed as (printfn "%A")(3)(+3)

/// Let binding

let x1 = 3

printfn "%A" x1

let plus(x,y) =  x + y

printfn "%A" (plus (3,4))

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

let ratio2(x,y) =
    let z = x * y
    let w = (2.0 * x) + ((2 : double)*y)
    w / z

printfn "%A" (ratio2(1,225))
printfn "%A" (ratio2(15,15))

// Conditional EXPRESSIONS (the equivalent of "condition ? e1 : e2" in languages derived from C)

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

let fn1 = plus' 3

// Q: when you write f1 (2,3), how many arguments does f1 get? Of which type?

let pair = (2,3)

let plus'' (x,y) = plus' x y

// Tuples

let triplet = (6,1,32)

let quadruple = (3,4,23,12)

// Q: what is the following value?

let quadruple' = (3,(4,23),12)

printfn "%A" (fst (1,2))
printfn "%A" (snd (1,2))

// Functional programming

let functional1 f x = 1 + (f x)

// Q: what does functional2 do in the following?
let functional2 f x = (functional1 f x) - 1

let succ x = x + 1

printfn "%A" (functional2 succ 3)

let functional3 = functional2 succ

printfn "%A" (functional3 3)

// Lists

// Arrays

// Pattern Matching

// Records

// Abstract data types

// Type variables