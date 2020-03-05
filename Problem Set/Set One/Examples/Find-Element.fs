module SetOne.Find_Element

//Program is designed to check if an item is present in the list, returns true if item is found
//otherwise a false is returned

let findItem list item =
    let rec recFindItem list =
        match list with
        | [] -> false
        | head::tail ->
            match head = item with
            | true -> true
            | false -> recFindItem tail
    recFindItem list
    
let runner =
     let list = [1..10]
     let charList = ['a'..'d']
     printfn "Is element %d present in list %A? %b" 1 list (findItem list 1)
     printfn "Is element %d present in list %A? %b" 11 list (findItem list 11)
     printfn "Is element %c present in list %A? %b" 'a' charList (findItem charList 'a')
     printfn "Is element %s present in list %A? %b" "\\n" charList (findItem charList '\n')

     

     