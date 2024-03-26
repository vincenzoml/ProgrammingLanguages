// First examples in FSharp

// This file is not meant to be run directly with "dotnet run" since it also contains errors
// Evaluate it line by line using the interpreter, or comment all the lines and then
// uncomment just one line and use dotnet run

// Comments are like this one


// Printf in all flavours



printfn "Hello world" (* Hello world -- Yes, one can also comment in this way *)

printfn "%d" 3 
printfn "%s" "hello"
printfn "The result of %d + %d is %d" 3 2 5
printfn "%A %A %A" 3.0 3 "3"

// Simple expressions

printfn "three plus three is %d" (3+3)
// Error:
// printfn "%d" 3+3
// because it's parsed as (printfn "%A")(3)(+3)

/// Let binding

let x = 3 + 3


printfn "%A" x

// Uncomment this to get an error
// let _ = (x : int)

let y = 3+3

printfn "%A" y

// Error:
// let x = 3+3
// At "top level", that is, outside of a function, no double declarations are permitted (because top-level symbols can be exported to other "modules", whatever they are)

// The above statement gives us an excuse to introduce functions

let f(x,y) =  x + y

printfn "%A" (f (3.0,4.0))

// Let's try a more complicated example

let g(x,y) =
    let z = x * y
    let w = (2*x) + (2*y)
    w 

// NOTE: language blocks are indentation-driven by default, but one can also make blocks explicit; we shall explore this if we need it
// See e.g. https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf S. 6.5 "Control Flow Expressions" page 91

printfn "%A" (g(1,225))
printfn "%A" (g(15,15))

// Is the output what you expected? If not why not?

// Let us get to types!

printfn "%A" ( (3).GetType().FullName )
printfn "%A" ( "3".GetType().FullName )

// Note: in vscode + ionide, you can already see the types without printing them, on the side as "fake comments" and hovering the mouse over a bound name.
let a = "ciao"

// Note: in the interpreter, types are printed after each evaluation! Try that.

// Question: why does the following raise an error?
// printfn "%A" ( 3.GetType().FullName )

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

let h(x,y) =
    let z = x * y
    let w = (2.0 * x) + ((2 : float)*y)
    w / z

printfn "%A" (h(1,225))
printfn "%A" (h(15,15))

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

// COMING SOON:

// Tuples vs functional abstraction

// Lists

// Arrays

// Pattern Matching

// Records

// Abstract data types