module SetOne.Add_Or_Sum

//This program will prompt the user to enter a a number and then it will add all
//The numbers starting at n to 1. 
let rec Addition a =
    match a with 
    | 0 -> 0
    | n -> n + Addition (n-1)
    
let rec Multiplication a =
    match a with
    | 1 -> 1
    | n -> n * Multiplication (n-1)

//Function will prompt the user to enter a number n
// and the operation desired, will validate the input and returns a tuple with the
// n number and the operation desired 
let rec promptUser () =
    let nInput  =
        printf "Enter whole number: "
        System.Console.ReadLine () |> int
    let operator =
        printf "Enter desired Operation (+,*): "
        System.Console.ReadLine ()
    match operator with
    | "+" ->
        printf "Sum from 1 to %d is " nInput
        Addition,nInput
    | "*" ->
        printf "Product from 1 to %d is" nInput
        Multiplication,nInput
    | _ -> promptUser()
//Runner code    
let runner =
    let operation, n = promptUser()
    printfn  "%d" (operation n)
    