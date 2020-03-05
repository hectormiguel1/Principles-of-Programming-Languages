module RecSum
let INVALID_INPUT = -1
let BASE_CASE = 0
//Prompt user to enter a number.
let prompt = 
    printf "Enter a number: "
    let number =
        match System.Int32.TryParse(System.Console.ReadLine()) with
        | (true, number) -> number
        | (false, _) -> INVALID_INPUT
    number

//Function adds from 1 to n using recursion
let rec sum number =
    match number with
        | 0 -> BASE_CASE
        | n -> n + sum (n-1)  
    
//Control function, prompts the user and prints final results
let runner =
    let userInput = prompt
    let finalSum = sum(userInput)
    printfn "Sum from 1 to %d is %d" userInput finalSum
    