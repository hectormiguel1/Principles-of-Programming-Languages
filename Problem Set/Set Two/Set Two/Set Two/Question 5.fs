module Set_Two.Question_5

let rec List_Length list =
    match list with
    | [] -> 0
    | x::xs -> 1 + List_Length xs 
    
//Tail Recursive     
let rec inner vector1 vector2 adder =
    match vector1 with
    | [] -> adder
    | x::xs -> match vector2 with
                | y::ys -> inner xs ys (adder + (y * x))
                | [] -> failwith "List length mismatch!"
 
//Non tail recisive function  
let rec inner2 vector1 vector2 =
    match vector1 with
    | [] -> 0I
    | x::xs -> match vector2 with
                | y::ys -> x * y + inner2 xs ys
                | [] -> failwith "List Length mismatch!"
                
let tester =
    let vector1 = [1I..50000I]
    let vector2 = [50001I..100000I]
    if (List_Length vector1) <> (List_Length vector2) then failwith "List Length Mismatch!"
    else
        let product1 = inner vector1 vector2 0I
      //  let product2 = inner2 vector1 vector2
        printf "Product is: %A" product1