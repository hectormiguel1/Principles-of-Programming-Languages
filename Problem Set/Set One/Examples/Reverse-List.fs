//Function takes a list and using recursion will reverse the list
//by appending the current tail to the new empty list module SetOne.ReverseList

module SetOne.ReverseList
 
let rec reverseListRec list reversedList = 
  match list with 
     | [] -> reversedList
     | head::tail -> reverseListRec tail (head::reversedList) 
  
//External wrapper function, does initial check for an empty list
//if list is not empty, calls the recursive function      
let reverseList list =
    match list with 
        | [] -> []
        | _ -> reverseListRec list []

//Runner expression used to test the code and make sure that everything works as intended          
let runner =
    let list = [1..5]
    printfn "List %A Revered is %A" list (reverseList list)