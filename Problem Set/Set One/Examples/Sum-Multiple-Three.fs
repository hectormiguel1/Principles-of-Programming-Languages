module Set_One.Sum_Multiple_Three

let INVALID_INPUT = 0
let BASE_CASE = 0
//Prompt user to enter a number.
let prompt () = 
    printf "Enter a number: "
    let input = System.Console.ReadLine()
    //Convert input string to int
    input |> int 

let printResults n finalSum =
    printfn "Sum from 1 to %d is %d (using only multiples of 3 or 5)" n finalSum
    
//Function adds from 1 to n using recursion, only taking into account multiples of 3 and 5
let rec sum number =
    if(number % 3 = 0 || number % 5 = 0) then
        match number with
        | 0 -> BASE_CASE
        | n -> n + sum (n - 1)
    else
        sum (number - 1)

//Control function, prompts the user and prints final results
let runner =
    let userInput = prompt()
    match userInput with
    | 0 -> ()
    | n -> printResults n (sum n)
