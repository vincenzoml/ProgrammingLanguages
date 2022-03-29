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

/// Product of the first k numbers

/// Sum for i in [1,k] of f(i) 

/// Sum for i in [1,k] of f(i), defined using a function, taking as arguments k and f

/// Same for product

/// Now take also the sum or product as an argument, and take an interval that is: 
// define a function fold taking three arguments: a binary function g, a unary function f, two extremes k1 and k2, a "initial value" z (which is 0 for the sum and 1 for the product) 
// "fold g f k1 k2 z" must compute g(g(z,f(k1)),f(k1+1),...), for instance "fold sum f 1 n 0" is the sum for i in [1,n] of f(i).

// Lists

let list1 = [1; 2; 3]

let list2 = [1,2,3]
let list2' = [(1,2,3)]

let list3 = (1,2,3)

// Q: What is the difference?

let list4 = [] // empty list

printfn "%A %A %A" list1.[0] list1.[1] list1.[2]

printfn "%A %A" (List.head list1) (List.tail list1)

let rec length lst = 
    if lst = [] 
    then 0
    else 1 + (length (List.tail lst))

printfn "%A" (length list1)
printfn "%A" (length list2)
printfn "%A" (length list2')

// "Cons" operator (see LISP)

let list5 = 4::list1

let list6 = 4::5::list1

let list6' = 4::(5::list1) // it's the same

let rec evenPositionsRec i lst =
    if lst = [] then []
    else 
        if i % 2 = 0 
        then (List.head lst)::(evenPositionsRec (i+1) (List.tail lst))
        else evenPositionsRec (i+1) (List.tail lst)

printfn "%A" (evenPositionsRec 0 [1;2;3;4;5])

let evenPositions lst = evenPositionsRec 0 lst

// ALSO:
let evenPositions' lst =
    let rec aux i lst =
        if lst = [] then []
        else 
            if i % 2 = 0 
            then (List.head lst)::(aux (i+1) (List.tail lst))
            else aux (i+1) (List.tail lst)
    aux 0 lst

/// Failure?
/// 

let rec firstElement (lst : list<int>) =
    if lst <> [] then List.head lst
    else failwith (sprintf "Error: there are no elements in the list %A" lst)

/// Note: fsharp like csharp and java has a sophisticated exception handling mechanism that we are just deliberately ignoring and super-simplifying

/// Exercises

/// Function to reverse a list
/// 
/// Function to sum all the elements of a list
/// 
/// Function to find the maximum of a list
/// 
/// Function to find the minimum of a list
/// 
/// Function to find both maximum and minimum in one pass
/// 
/// Function to "intersperse" a list with a value, e.g.
/// intersperse "-" ["a";"b";"c"] = ["a";"-";"b";"-";"c"]
/// 
/// Note: after "c" there should NOT be an occurrence of "-"
/// 
/// Function to concatenate two lists
/// concat [1;2] [3;4] = [1;2;3;4]
/// 
/// Function to multiply two lists value by value; raise an error if one is longer than the other one. 
/// multiplyLists [1;2;3] [4;5;6] = [4;10;18]

// Function to "zip" a list: from a pair of lists, obtain a list of pairs

// Function to unzip a list: from a list of pairs, obtain two lists

// Don't look at the solution















let rec unzip l = 
    if l = [] then ([],[])
    else 
        let hd = List.head l
        let tl = unzip (List.tail l)
        ((fst hd)::(fst tl),(snd hd)::(snd tl))

// Pattern matching (start by two chained ifs)

// Arrays

// Records

// Abstract data types

// Type variables