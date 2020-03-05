module Set_Two.Question_6
//Attempted to make as many tail recursions as possible

//Function in charge of doing the maths, ie vector multiplication 
let rec inner vector1 vector2 adder =
    match vector1 with
    | [] -> adder
    | x::xs -> match vector2 with
                | y::ys -> inner xs ys (adder + (y * x))
                | [] -> failwith "List length mismatch!"
 
 //Used to compute the number of rows, useful for checking that the matrix multiplication can be carried out.                
let rec getNumberOfRows numRows = function
    | [] -> numRows
    | x::xs -> getNumberOfRows (numRows + 1) xs

//This function is used to make our life easier, simply transpose the second matrix and not its just several
//vector multiplications.     
let rec transpose final matrix =
    match matrix with
    | (x::y)::ys -> let current = List.map List.head matrix
                    let remainder = (List.map List.tail matrix)
                    transpose (final@[current]) remainder
    | _ -> final 
//build the new matrix by building each row, ie, do the vector math for each column and then make that into a list, that is
//the row, then combine those to make the new matrix.    
let rec buildMatrix finalMatrix secondMatrix = function
    | [] -> finalMatrix
    | x::xs -> let currentRow = buildRow [] x secondMatrix
               buildMatrix (finalMatrix@[currentRow]) secondMatrix xs
    
and buildRow finalRow matrix1Row matrix2 =
    match matrix2 with 
    | [] -> finalRow
    | y::ys ->  let currentDotProduct = inner matrix1Row y 0
                buildRow (finalRow@[currentDotProduct]) matrix1Row ys
 
//Actual multiplication function, carries out a check to make sure that the matrix multiplication can be done,
//then proceeds to build the new matrix, using the functions above.              
let multiply (matrix1, matrix2) =
    let numColumsMatrix2 = List.length (List.head matrix2)
    let numRowsMatrix1 = getNumberOfRows 0 matrix1
    
    if numColumsMatrix2 <> numRowsMatrix1
    then failwith "Unable to do Matrix Multiplication, rows matrix one do not match columns in matrix 2"
    else
        buildMatrix [] (transpose [] matrix2) matrix1
 //Actual runner code.        
let runner =
    let matrix1 = [[1;2;3];[4;5;6]]
    let matrix2 = [[0;1];[3;2];[1;2]]
    printf "Matrix 1: %A \n" matrix1
    printf "Matrix 2: %A \n" matrix2
    printf "Matrix 2 Transposed: %A \n" (transpose [] matrix2)
    printf "Matrix Multiplication of Matrix 1 and 2: %A \n" (multiply (matrix1, matrix2))
    