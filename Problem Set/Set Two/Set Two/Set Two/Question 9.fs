module Set_Two.Question_9

//Function to return last time in the list, tail recursive. 
let rec lastitem = function
    | [] -> None
    | [x] -> Some x
    | x::xs -> lastitem xs

//Error handler, general.  
let errorHandler = function
    | None -> "Invalid Input"
    | Some x -> sprintf "%A" x

//Actual driving code. 
let runner =
    [] |> lastitem |> errorHandler |> printf "%s \n"
    ["cat"] |> lastitem |> errorHandler |> printf "%s \n"
    [1;2;3;4;5] |> lastitem |> errorHandler |> printf "%s \n"


    