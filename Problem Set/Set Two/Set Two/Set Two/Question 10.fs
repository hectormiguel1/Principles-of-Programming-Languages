module Set_Two.Question_10

type Exp =
      Num of int
    | Neg of Exp
    | Sum of Exp * Exp
    | Diff of Exp * Exp
    | Prod of Exp * Exp
    | Quot of Exp * Exp
    | Paren of Exp 
    
let rec evaluate = function
    | Num n -> Some n
    | Neg e -> match e with
                | Num n -> Some (-n)
                | _ -> let innerExpression = evaluate e
                       if innerExpression.IsSome then Some(-innerExpression.Value) else None 
    | Sum(exp1,exp2) -> let num1 =  evaluate exp1                                     
                        let num2 = evaluate exp2                                    
                        if num1.IsSome && num2.IsSome then Some (num1.Value + num2.Value) else None
    | Diff(exp1,exp2) -> let num1 = evaluate exp1                              
                         let num2 = evaluate exp2                                  
                         if num1.IsSome && num2.IsSome then Some (num1.Value - num2.Value) else None 
    | Prod(exp1, exp2) -> let num1 = evaluate exp1                  
                          let num2 =  evaluate exp2                          
                          if num1.IsSome && num2.IsSome then Some (num1.Value * num2.Value) else None 
    | Quot(exp1, exp2) -> let num1 =  evaluate exp1
                          let num2 =  evaluate exp2
                          if num1.Value <> 0 && num2.Value <> 0 then Some (num1.Value / num2.Value) else None
    | Paren exp -> evaluate exp 
 
                            
let errorHandler = function
    | None -> "Invalid Input, Div by 0"
    | Some x -> sprintf "%A" x 
                       
                       
let runner =
    (Prod(Num 3, Diff(Num 5, Num 1))) |> evaluate |> errorHandler |> printf "%s \n "
    (Diff(Num 3, Quot(Num 5, Prod(Num 7, Num 0)))) |> evaluate |>  errorHandler |> printf "%s \n "
                        
    
    