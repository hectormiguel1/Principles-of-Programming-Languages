module SetOne.Examples._21

    let gencut(size, list) =
        let rec split term firstSection secondSection =
            match term with
            | 0 -> firstSection::[secondSection]
            | n ->
                match secondSection with
                | [] -> []
                | head::tail ->
                    split (n-1) (firstSection@[head]) tail
        split size [] list
        
    let interleave (list1, list2) =
        let rec recInterleave (x, y) appendList =
            match (x,y) with
            | ([],[]) -> appendList
            | (x::xs,y::ys) ->
                recInterleave(xs,ys) (appendList@[x]@[y])
        recInterleave (list1,list2) []

    let cut list =
         gencut((List.length list ) /2, list)
         
         
    let shuffle list =
        let splitList = cut(list)
        interleave(splitList.Head,splitList.Tail.Head)
        
    let countShuffles deckSize =
        let orig = [1..deckSize]
        let shuffled = shuffle orig
        let rec countAux (deck, target) =
            match deck = target with
            | true -> 1
            | false ->
                countAux((shuffle deck), target) + 1
                
        countAux (shuffled,orig)
        
    let runner =
        printfn "Shuffle count for 52 is: %d" (countShuffles 52)