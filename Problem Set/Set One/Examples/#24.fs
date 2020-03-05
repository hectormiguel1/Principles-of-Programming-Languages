module SetOne.Examples._24

let split (list :'a list list)  =
    (list.Head, list.Tail.Head)
    
//We are assuming that both lists are the same length, for this reason
//Only this 2 base cases are needed. 
let transpose list =
    let (m,n) = split list
    let rec rectranspose  = function
        | ([],[]) -> []
        | (headM::tailM, headN::tailN) -> [headM;headN]::rectranspose (tailM,tailN)
        
    rectranspose (m,n)
    
let runner =
    let list = [[1;2;3];[4;5;6]]
    printfn "List %A Transposed is: %A" list (transpose list )