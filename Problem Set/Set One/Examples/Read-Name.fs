module SetTwo
//Program will read name from user, and greet them 
//@author: Hector Ramirez

//Return Value when no error Occurs
let NO_ERROR = 0

//Start of the program
//Main function
let main =
//Prompt User
    printf "%s: " "Enter your name"
    //Read input from user
    let nameStr = System.Console.ReadLine();
    //print final result
    printfn "Hello %s" nameStr
    //return
    NO_ERROR

