module SetOne.Examples._20

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
        
    let runner =
        let testList = [1..10]
        printfn "Starting state of list: %A" testList
        printfn "After Shuffle: %A" (shuffle testList)