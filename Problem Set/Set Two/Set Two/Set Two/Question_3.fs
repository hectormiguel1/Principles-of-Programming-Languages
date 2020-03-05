module Set_Two.Question_3

//  S -> if E then S else S | begin S L | print E
//  L -> end | ; S L
//   E -> i

type TERMINAL = IF | THEN | ELSE | BEGIN | END | PRINT | SEMICOLON | ID | EOF

let accept () =
     printf "Input program was accepted and is valid \n"
let error () =
     printf "program not valid \n "
let matchNextTocken token nextToken  =
     if token = THEN || token = ELSE || token = BEGIN then
          match nextToken with
          | IF -> true
          | BEGIN -> true
          | PRINT -> true
          | _ -> false
          
     elif token = SEMICOLON then
          match nextToken with
          | IF -> true
          | BEGIN -> true
          | PRINT -> true
          | _ -> false 
     else false 
          
let rec S (tokens : TERMINAL list) =
     match tokens with
     | []  -> []
     | [x] -> if x = EOF then [TERMINAL.EOF] else []
     | x::y::xs -> match x with
                     | TERMINAL.IF -> if y = ID then S xs else []
                     | TERMINAL.THEN -> let isNextTockenCorrect = matchNextTocken x y
                                        if isNextTockenCorrect then  S (y::xs)
                                        else failwith "Incorrect Next token"
                     | TERMINAL.ELSE  -> let isNextTockenCorrect = matchNextTocken x y
                                         if isNextTockenCorrect then  S (y::xs)
                                         else failwith "Incorrect Next token"
                     | TERMINAL.BEGIN -> let isNextTockenCorrect = matchNextTocken x y
                                         if isNextTockenCorrect then  S (y::xs)
                                         else failwith "Incorrect Next token" 
                     | TERMINAL.PRINT -> if y = ID then S xs else []
                     | _ -> L (x::y::xs) 
               
and L tokens =
     match tokens with
     | [] -> []
     | [EOF] -> [EOF]
     | x::y::ys -> match x with
                    | END -> S(y::ys)
                    | SEMICOLON -> let isNextTockenCorrect = matchNextTocken x y
                                   if isNextTockenCorrect then  S (y::ys)
                                   else failwith "Incorrect Next token"
     
     
let test_program program =
      let result = program |> S
      match result with 
      | [] -> failwith "Early termination or missing EOF"
      | x::xs -> if x = EOF then accept() else error()
      

      
let runner =
     let program1 = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
     let program2 = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
     let program3 = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]
     printf "Program # 1: \n"
     test_program program1
     printf "Program # 2: \n"
     test_program program2
     printf "Program # 3: \n "
     test_program program3