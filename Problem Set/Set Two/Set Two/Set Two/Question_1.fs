module Set_Two.Question_1

//This function takes a non-curried function and converts it into a curried function
let curry functionToCurry = (fun x -> fun y -> functionToCurry(x,y))

//This Function takes a curried function and returns an un-curried function
let uncurry functionToUncurry = (fun (x,y) -> functionToUncurry x y)
    

//Tester function  
let runner =
    let uplus = uncurry (+)
    let cplus = curry uplus
    uplus, cplus 