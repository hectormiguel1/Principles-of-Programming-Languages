module Set_Two.Question_2

type Coordinates<'a> =
    | Two_D of width : 'a * length : 'a
    | Three_D of height : 'a * 'a * width : 'a
    | Four_D of time : 'a * 'a * 'a * height : 'a 

let mk_function binFunction (coordinate: Coordinates<'a>) =
    match coordinate with 
    | Two_D (n,m) -> binFunction n m 
    | Three_D (n,m,o) -> binFunction (binFunction n m)  o 
    | Four_D (n,m,o,p) -> binFunction (binFunction (binFunction n m)  o ) p
    
let intTouple = Two_D(1,2)
let floatThreeple = Three_D(2.0,3.0,4.0)
let stringFourple = Four_D("Hello","My","Name","is")

let runner =
    let test1 = mk_function (+) intTouple
    let test2 = mk_function (+) floatThreeple
    let test3 = mk_function (+) stringFourple
    let test4 = mk_function (-) intTouple
    let test5 = mk_function (-) floatThreeple
    printf "test 1: %d  test 2: %f  test 3: %s test 4: %d test 5: %f \n" test1 test2 test3 test4 test5