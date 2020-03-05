module Set_Two.Question_4

type TOKENS = ID | ADD | SUB | MUL | DIV | LPAREN | RPAREN | EOF

(*
    Rules are:
      E -> E + T | E - T | T
      T -> T * F | T / F | F
      F -> i | (E)
*)
let accept () =
     printf "Input program was accepted and is valid \n"
let error () =
     printf "program not valid \n "
     
let matchNextToken currentToken nextToken =
    if currentToken = ID || currentToken = RPAREN then
        match nextToken with
        | ADD -> true
        | SUB -> true
        | MUL -> true
        | DIV -> true
        | RPAREN -> true
        | EOF -> true 
        | _ -> false
        
    elif currentToken = LPAREN  then
        match nextToken with
        | ID -> true
        | _ -> false
     
     else false 
        
        
let rec E tokens =
    match tokens with
    | [] -> []
    | [x] -> if x = EOF then [EOF] else []
    | x::y::ys -> match x with
                    | ID -> let isNextTokenValid = matchNextToken x y
                            if isNextTokenValid then 
                             if y = ADD || y = SUB then E (ys)
                              elif y = MUL || y = DIV then E (ys)
                              elif y = RPAREN then E (y::ys)
                              elif y = EOF then E [y]
                              else failwith "Incorrect next token!"
                            else failwith "Incorrect next token!"
                    | LPAREN -> let isNextToeknValid = matchNextToken x y
                                if isNextToeknValid then E (y::ys) else failwith "Incorrect next token!"
                    | RPAREN -> let isNextTokenValid = matchNextToken x y
                                if isNextTokenValid then E(ys) else failwith "Incorrect next token!"
                    | _ -> failwith "Incorrect next token!"


let test_program program =
      let result = program |> E
      match result with 
      | [] -> failwith "Early termination or missing EOF"
      | x::xs -> if x = EOF then accept() else error()
      

      
let runner =
     let program1 = [ID;ADD;ID;ADD;ID;ADD;ID;EOF]
     let program2 = [ID;SUB;ID;MUL;ID;EOF]
     let program3 = [LPAREN;ID;SUB;ID;RPAREN;MUL;ID;EOF]
     let program4 = [LPAREN; ID; ID; ADD; ID]
     printf "Program # 1: \n"
     test_program program1
     printf "Program # 2: \n"
     test_program program2
     printf "Program # 3: \n "
     test_program program3
     printf "Program # 4: \n "
     test_program program4