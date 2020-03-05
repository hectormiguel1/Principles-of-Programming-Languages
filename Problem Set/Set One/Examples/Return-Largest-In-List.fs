module SetOne.Return_Largest_In_List

//function takes a list
//Inner recursvive function will then recusevly iterate through the list
//and then return the largest item on the list.
let rec findLargest list prevHead =
    match list with
    | [] -> prevHead
    | head::tail ->
        match head > prevHead with
        | true -> findLargest tail head
        | false -> findLargest tail prevHead

//Outer wrapper when dealing with Integers
let returnLargest list =
    match list with
    | [] -> 0
    | head::tail -> findLargest tail head
    
//Runner code    
let runner =
    let list :int list = [1;5;3;20;2;6]
    printfn "From list %A largest digit is %d " list (returnLargest list) 