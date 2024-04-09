// Folds

let rec foldLeft g f k1 k2 z = 
    if k1 >= k2 then g(z,f(k1))
    else g(foldLeft g f k1 (k2-1) z,f k2)

let rec foldRight g f k1 k2 z = 
    if k1 >= k2 then g(f(k1),z)
    else g(f k1,foldRight g f (k1+1) k2 z)

printfn "%A" (foldLeft (fun (a,b) -> a-b) (id) 1 5 0)

printfn "%A" (foldRight (fun (a,b) -> a-b) (id) 1 5 0)

// Tuples

// Demonstrate the usage of tuples with different element types, to pass different arguments to a function

let f ((x,y) : int * string) =
    (string x) + y

f (3,"test")

p



// Lists

let list1 = [1; 2; 3]

let list2 = [(1,2,3)]

let list3 = (1,2) // TENERE BEN PRESENTE LA DIFFERENZA FRA list3, list3' e list3''

let list3' = [1,2] // lo stesso di [(1,2)]

let list3'' = [1;2] // NB il significato di [(1;2)] con le parentesi è estremamente diverso (";" è il costrutto di sequenza)

// Q: What is the difference?

let list4 = [] // empty list

printfn "%A %A %A" list1[0] list1[1] list1[2]

printfn "%A %A" (List.head list1) (List.tail list1)

let rec length lst = 
    if lst = [] 
    then 0
    else 1 + (length (List.tail lst))

let rec sum lst =
    if lst = [] then 0 
    else (List.head lst) + (sum (List.tail lst))

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

// evenPositionsRec 1 [7;12] 0 = evenPositionsRec 2 [12] = 12 :: (evenPositionsRec 3 []) = 12 :: [] = [12] 



printfn "%A" (evenPositionsRec 0 [1;2;3;4;5])



let evenPositions lst = evenPositionsRec 0 lst

// > evenPositions ["a";"b";"c";"d";"e"];;
// val it: string list = ["a"; "c"; "e"]

// POTEVO ANCHE SCRIVERE 
// let evenPositions = evenPositionsRec 0

// ALSO:
let evenPositions' lst =
    let rec aux i lst =
        let x = lst
        printfn "Debug: %A" x    
        if lst = [] then []
        else 
            if i % 2 = 0 
            then (List.head lst)::(aux (i+1) (List.tail lst))
            else aux (i+1) (List.tail lst)
    aux 0 lst

/// Failure?
/// 

let f x y = 
    if y = 0 
    then failwith "divisione per zero"
    else x / y 

let rec firstElement (lst : list<int>) =
    if lst <> [] then List.head lst
    else failwith (sprintf "Error: there are no elements in the list %A" lst)

/// Note: fsharp like csharp and java has a sophisticated exception handling mechanism that we are just deliberately ignoring and super-simplifying

/// Exercises

/// Function to reverse a list
/// 

let rec reverseRec acc lst =
    if lst = [] then acc
    else reverseRec ((List.head lst) :: acc) (List.tail lst)

let reverse lst = reverseRec [] lst
/// 
/// Function to sum all the elements of a list
/// 
/// Function to find the maximum of a list
/// 
/// Function to find the minimum of a list
/// 
/// Function to find both maximum and minimum in one pass
/// 
/// Function to find the leftmost local optimum
/// 
/// Function to find the rightmost local optimum
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
/// 
/// 

let rec concat l1 l2 =
    if l1 = [] 
    then l2 
    else (List.head l1)::(concat (List.tail l1) l2)

let rec multiplyLists l1 l2 =
    if l1 = [] && l2 = [] 
    then []
    else 
        if l1 = [] || l2 = [] 
        then failwith "Error: one of the two lists is empty" 
        else ((List.head l1) * (List.head l2))::(multiplyLists (List.tail l1) (List.tail l2))

/////// LECTURE 4 ENDS HERE

/////// LECTURE 5 STARTS HERE

// Pattern matching 
let rec fib x = // THIS IS TEDIOUS!
    if x = 1 
    then 1
    else 
        if x = 2 
        then 1
    else (fib (x-1)) + (fib (x-2)) 

let rec fib x = 
    (match x with    
        | 1 -> 1
        | 2 -> 1
        | n -> (fib (n-1)) + (fib (n-2))
    )

let example x = 
    match x with
    | (a,b) -> a + b



//// TEMPLATE
// match ESPRESSIONE with  -- espressione è il valore da decostruire
//     | MATCH1 -> RISULTATO1 
//     ...
//     | MATCHN -> RISULTATON


let rec fib2 x = 
    match x with    
    | n when n < 1 -> failwith (sprintf "Error: %d is less than 1)" x)
    | (1|2) -> 1
    | m -> (fib2 (m-1)) + (fib2 (m-2))

let rec fn2 a =
    match a with
    | (x,y) when x > 0 && y > 0 -> x + y
    | (x,_) when x < 0 -> 0    
    | _ -> failwith "this makes no sense"

let fst a =
    match a with
    | (x,_) -> x

let rec length2 l =
    match l with    
    | [] -> 0
    | (x :: xs) -> 1 + (length2 xs)

let rec sumMatching l =
    match l with
    | [] -> 0 
    | (x::xs) -> x + (sumMatching xs)
    
let sumOfThreeElements l =
    match l with
    | [x] -> x+y+z
    | failwith "the list doesn't have three elements"

let rec nth i l = 
    match (i,l) with
    | (_,[]) -> failwith "empty list"
    | (0,x::_) -> x 
    | (n,_::xs) when n > 0 -> nth (n-1) xs
    | _ -> failwith "i is negative"

let rec nth' i l = 
    if i > 0 then 
        match (i,l) with
        | (_,[]) -> failwith "empty list"
        | (0,x::_) -> x 
        | (n,_::xs) -> nth (n-1) xs
    else failwith "i is negative"
        
let rec myMax1(a,b) = 
    match (a,b) with
    | _ when a >= b -> a
    | _ -> b

let rec fst'(a,_) = 
    match 13 with
    | _ -> a

// Incomplete matches
let f4 x = 
    match x with
    | 0 -> "zero"    

// Exercise: implement Euclid's algorithm
let rec eucl x =
    match x with
    | (a,b) when a <= 0 || b <= 0 -> failwith "non-positive number detected"
    | (a,b) when a > b -> eucl (a-b,b)
    | (a,b) when a < b -> eucl (a,b-a)
    | (a,_) -> a



// Implement concatenation via pattern matching

// Look at the List module

// Function to "zip" a list: from a pair of lists, obtain a list of pairs

let rec zip l1 l2 = 
    match (l1,l2) with
    | ([],[]) -> []
    | (x::xs,y::ys) -> (x,y)::(zip xs ys)
    | _ -> failwith "zip cannot operate on lists of different lengths"


let rec zip' l1 l2 = 
    match (l1,l2) with
    | ([],_)|(_,[]) -> []    
    | (x::xs,y::ys) -> (x,y)::(zip xs ys)
    
// Function to unzip a list: from a list of pairs, obtain two lists

let rec unzip l = 
    match l with
    | [] -> ([],[])
    | (a,b)::tl -> 
        let (h,k) = unzip tl
        (a::h,b::k)

///// END OF LECTURE 5 

// Arrays (NOT SEEN IN CLASS)

let v1 = [|0;1;2|]
let v2 = [||]

printfn "%A"
    (match v1 with 
        | [||] -> 0
        | [|x1;x2|] -> x1)

printfn "%A" v1.[2]

printfn "%A" [|10;12;3|].[3]

printfn "%A" [1;2;3].[2]

// What's the difference between lists and arrays??

// 1) Constant vs linear access time
// 2) Arrays can be modified in place (with constant access time)

let array1 = [| 1; 2; 3 |]
array1.[0] <- 7

printfn "%A" array1.[0]

// However...

let listOfArrays = [ [| 0 |]; [| 1 |]; [| 2 |] ]

listOfArrays.[0].[0] <- 12

printfn "%A" listOfArrays.[0].[0]

// In the terminal: 
// > listOfArrays;;
// val it: int[] list = [[|12|]; [|1|]; [|2|]]

// Exercise: define the component-wise product of two arrays and of two lists

// Define the dot product of two arrays and of two lists

// Define the scalar product between a scalar and an array

// Check the module Array

// Check in particular the map and iter functions

let list7 = List.ofArray array1

let list8 = List.map (fun x -> x+1) list7

printfn "%A\n%A" list7 list8

List.iter (fun x -> printfn "--- %A ---" x) list8

// Array.map and Array.iter work in the same way

// We have also fold, and foldBack
printfn "%A" (List.fold (fun x y -> x + y) 0 list8)
 
// Exercise:

// Define a function which has several local maxima e.g. 
// let f1 (x : int) =                        
//     let y = float x
//     let res = -(15.0 * (cos (4.0*y)) - 3.0*y)
//     int res

// Or simpler: 
// let f2 x = -(15.0 * (cos (4.0*y)) - 3.0*y)

// Define a function that tabulates a function on the integer in a closed interval and returns a list
// tabulate i j f = [f i,f (i+1),...,f j]
// if i> j then the empty list must be returned

// Use List.fold and List.foldBack to find the first and last local maxima of f1 over an interval

// Find the global maximum on a list

// Find in one pass the maximum and minimum (use a pair as the state of the fold)

// Use foldBack to unzip list of pairs to a pair of lists (use a list as the state of the fold)
 
let unzip2 l = 
    List.foldBack
        (fun x st -> 
            match st with
            | (l1,l2) -> (fst x::l1, snd x::l2))        
        l
        ([],[])

// Type variables and parametric types

let interestingFunction x y = printfn "%A %A" x y
// Error: it's not clear what is the type of l (e.g. array or list) due to overloading
// let interestingFunction2 l = l.[0]
let interestingFunction2 (l : list<'a>) = l.[0]

// Type abbreviations

type UsefulType = int * string

let f (x: UsefulType) = 
    match x with
    | (a,b) ->
        sprintf "Useful: (%d,%s)" a b


// Errors:
// type UsefulType2 = a * string // a is an undefined type
// type UsefulType2 = 'a * string // a is an undefined type parameter

type UsefulType<'a> = 'a * string

// Warning: 3 is less generic than 'a
let ut1 : UsefulType<'a> = (3,"ciao")

// How to make it more generic?
let f (x : 'a) = (x,"ciao") : UsefulType<'a>


// But UsefulType is not that useful. It's just a type synonym.

let ut2 = ut1 : int * string

let fnUT1 (x : UsefulType<'a>) = fst x

// Type constructors create fresh types, distinct from all the others

type VeryUseful = VU of int

let vu1 = VU 3

let fnVU1 x = 
    match x with 
    | VU i -> i + 1

let fnVU2 x = 
    match x with 
    | VU i -> VU (i + 1)



type Useful2 = Case1 of int | Case2 of string

let fn x =
    match x with
    | Case1 a -> sprintf "%d" a
    | Case2 b -> b

let fnStrange x =
    match x with
    | Case1 a -> Case1 (a+1)
    | Case2 b -> Case2 (b + "_incremented")

// > [Case1 3; Case2 "ciao"];; 
// val it: Useful2 list = [Case1 3; Case2 "ciao"]

type Alternative<'a,'b> = Alt1 of 'a | Alt2 of 'b

// Pattern matching!
// For more info, see https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching and google.
let applyAlt x f1 f2 = 
    match x with
    | Alt1 x1 -> f1 x1
    | Alt2 x2 -> f2 x2

let mapAlt (x : Alternative<'a,'b>) f1 f2 =
    match x with
    | Alt1 x1 -> Alt1 (f1 x1)
    | Alt2 x2 -> Alt2 (f2 x2)

let map1Alt x f =
    match x with
    | Alt1 x1 -> Alt1 (f x1)
    | Alt2 x2 -> Alt2 (f x2)



// Q: What does the following do instead
// let Alt2 el2 = alt1

// Imperative programming in FSharp

// References

let mutable ref1 = 0

ref1 <- 1

printfn "%A" ref1

let mutableList = [ref1]

// But....
// mutableList.[0] <- 1

let ref2 = ref 0

ref2.Value <- 1

// Q: what does y.Value = 1 mean instead?

let refList = [ref2]

refList.[0].Value <- 28

printfn "%A" refList

printfn "%A" (List.map (fun (r : ref<'a>) -> r.Value) refList)


// Exercise: explain and demonstrate the difference between:

let refList2 = 
    let r = ref 0
    [r,r]

let refList3 = 
    [ref 0,ref 0]

// Wow, so expanding name definitions does not work?
// In the presence of side effects, this is true.

// More imperative programming


let idxOf x vector = 
    if Array.length vector <= 0 then failwith "undefined"    
    let mutable found = false
    let mutable result = -1
    let mutable index = 0
    while not found do
        if index >= Array.length vector then failwith "not found"
        if vector.[index] = x then 
            result <- index
            found <- true
        else 
            index <- index + 1
    result

let v = [| 17; 42; 33; 57; 12; 1; 100; 22; 0|]

printfn "%A" (idxOf 34 [||])

// Exercise: find the maximum using while

// Imperative programming 

let max1 vector =
    if Array.length vector <= 0 then failwith "undefined"    
    let mutable res = vector.[0]
    for i = 1 to (Array.length vector) - 1 do
        res <- max res vector.[i]
    res

// Exercise: pass the comparison function as an argument

// Let's mix functional and imperative style...

let max2 vector (res : ref<int>) =
    if Array.length vector <= 0 then failwith "undefined"    
    res.Value <- vector.[0]
    Array.iter (fun x -> res.Value <- max res.Value x) vector
        
// Q: how to retrieve the result after calling
// max2 v (ref 0)

// Recursive types!

type List<'a> = Empty | Cons of ('a * List<'a>)

let lst1 = Cons (3, Cons (4, Cons (7,Empty)))

let rec sumList l =
    match l with
    | Empty -> 0
    | Cons (head,tail) -> head + (sumList tail)

printfn "%A" (sumList lst1)

// Recursive types are a classical topic in set theory:
type Int = Zero | Succ of Int

let sem : Int -> int = 
    fun x -> 
        match x with
            | Zero -> 0
            | Succ y -> y + 1




// Exercise: define the sum of Int using the above declaration
let rec sum (x : Int) (y : Int) = 
    match x with
    | Zero -> y
    | Succ a -> Succ (sum a y)

type ListInt = Empty | Cons of (int * ListInt)

let x = Empty

// Pattern matching su ADT
let esempio l = 
    match l with
    | Empty -> "vuoto"
    | _ -> "pieno"


let esempio2 l =
    match l with
    | Empty -> "vuoto"
    | Cons (_,Empty) -> "un elemento"
    | _ -> "più di un elemento"

// Esempio
let x = Cons(3, Cons (4, Cons (12,Empty)))

// Alberi binari

type BinTree<'a> = Empty | Node of ('a * BinTree<'a> * BinTree<'a>)

let x = Node (3, Node (4, Empty, Empty), Node (5,Empty, Empty))

let rec maxDepth bt =
    match bt with
    | Empty -> 0
    | Node (x,y,z) -> 1 + (max (maxDepth y) (maxDepth z))

printfn "%A" (maxDepth (
                    Node ("ciao",
                        Node("a",Empty,Empty),
                        Node ("b",
                                Node ("c",Empty,Empty),
                                Empty)
                    )))


// Recall how to do pattern matching on n-uples
// let x = (1,"a",true)

// match x with 
//     | (x,y,z) -> ...