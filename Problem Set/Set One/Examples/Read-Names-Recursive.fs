module Three
//Program will read name from user, and greet them 
//@author: Hector Ramirez

//Return Value when no error Occurs
let NO_ERROR = 0

//Greet Function
let greet name =
    printfn "Hello %s" name

//Prompt Function
let rec prompt () =
    printf "Enter a name to be greeted (Expecting Alice or Bob): "
    let promptName = System.Console.ReadLine();
    match promptName with
    | "Alice" -> greet "Alice"
    | "Bob" -> greet "Bob"
    | _ -> prompt()


//Start of the program
//Main function
let main =
    prompt()
    prompt()
    prompt()
