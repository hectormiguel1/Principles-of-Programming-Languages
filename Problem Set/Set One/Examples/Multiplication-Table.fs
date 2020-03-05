module SetOne.Multiplication_Table


(*
    This function will create a separator string ("-") of the length sent into
    the function. The String will be generated using recursion and concatenating the
    recursion calls into one string to be returned at the end
*)
let separatorBuilder numDashes =
    let rec stringBuilder currentIter =
        match currentIter with
        | 0 -> ""
        | n -> "-" + stringBuilder (n-1)
    "" + stringBuilder numDashes
(*
    This function will take a number and print the multiplication table from 1 to number
    The table will be generated recursively and a seperator will be introduced in-between lines for readability. 
 *)
let MultiTable largestMultiple =
    let separator = separatorBuilder (largestMultiple * 4)
    let rec MultiplicationTable line =
        let rec doLine number =
            match number with
            | 0 -> ()
            | n ->
                doLine(n - 1)
                printf "%3d " (line * n)

        match line with
        | 0 -> ()
        | n ->
            MultiplicationTable (n - 1 )
            doLine largestMultiple
            printfn "\n%s" separator
    MultiplicationTable largestMultiple

//Actual run code
MultiTable 13
    
    
