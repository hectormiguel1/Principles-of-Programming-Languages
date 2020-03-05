module SetOne.Examples._18

    let interleave (list1, list2) =
        let rec recInterleave (x, y) appendList =
            match (x,y) with
            | ([],[]) -> appendList
            | (x::xs,y::ys) ->
                recInterleave(xs,ys) (appendList@[x]@[y])
        recInterleave (list1,list2) []
    
    let runner =
        let list1 = [1;2;3]
        let list2 = [3;4;5]
        printfn "Interleave List 1 %A with List 2 %A = %A" list1 list2 (interleave (list1,list2))