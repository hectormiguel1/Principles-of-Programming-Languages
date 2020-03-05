module SetOne.Examples._23
let rec powerset = function
                   | [] -> [[]]
                   | h::t -> List.fold (fun xs t -> (h::t)::t::xs) [] (powerset t)
                   
let runner =
    let list1 = [1..3]
    printfn "Power set of list %A is %A" list1 (powerset list1)
        
