module SetOne.Examples._19

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
        
    let cut list =
         gencut((List.length list ) /2, list)
         
    let runner =
        let list = [1..6]
        printfn "List 1: %A cut in half is %A" list (cut list)
        