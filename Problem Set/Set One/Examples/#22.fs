module SetOne.Examples._22

    let cartesian (list1, list2) =
        let rec recCartProduct (listA, listB) workingList =
            match (listA, listB) with
            | ([],[]) -> workingList
            | (head::tail, []) -> recCartProduct (tail, listB.Tail) (workingList@[(head, listB.Head)])
            | ([], head::tail) -> recCartProduct (listA.Tail,tail) (workingList@[(listA.Head,head)])
            | (headA::tailA, headB::tailB) ->
                recCartProduct(tailA,tailB) (workingList@[(headA,headB)])
        recCartProduct (list1,list2) []
    
    let runner =
        let list1 = ["a";"b";"c"]
        let list2 = [1..4]
        printfn "Cartesian Product of %A and %A is %A" list1 list2 (cartesian (list1,list2))