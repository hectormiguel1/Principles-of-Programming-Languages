module SetOne.Examples._17

    let revLists list =
        List.map (List.rev) list
        
    let runner =
        let list = [[0;1;1];[3;2];[];[5]]
        printfn "%A reversed is %A" list (revLists list) 