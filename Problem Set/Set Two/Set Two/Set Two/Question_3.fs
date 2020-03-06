module Set_Two.Question_3
type TERMINAL = IF | THEN | ELSE | BEGIN | END | PRINT | SEMICOLON | ID | EOF
//Change names to be more unique,
// Add ends and semicolon branches 
let E = function
    | [] -> None
    | ID::xs -> Some(xs)
    | _ -> None
let eat terminal = function
    | x::xs when x = terminal -> Some(xs)
    | _ -> failwith "incorrect token obtained"
let rec S = function
    | [] -> None
    | [EOF] -> None
    | IF::xs -> let tmpHolder = xs |> E
                let tail = if tmpHolder <> None then tmpHolder.Value else []
                let tmpHolder = (tail |> eat THEN).Value |> S
                let  tail = if tmpHolder <> None then tmpHolder.Value else []
                let tmpHolder = (tail |> eat ELSE).Value |> S
                let tail = if tmpHolder <> None then tmpHolder.Value else []
                Some(tail)
    | BEGIN::xs -> let tmpValue = xs |> S
                   let tail = if tmpValue <> None then tmpValue.Value else []
                   let tmpValue = tail |> L
                   let tail = if tmpValue <> None then tmpValue.Value else []
                   Some(tail)
    | PRINT::xs -> let tmpHolder = xs |> E
                   let tail = if tmpHolder <> None then tmpHolder.Value else []
                   Some(tail)
    | _ -> None 
and L = function
    | [] -> None
    | END::xs -> Some(xs)
    | SEMICOLON::xs -> let tmpHolder = xs |> S
                       let tail = if tmpHolder <> None then tmpHolder.Value else []
                       let tmpHolder = tail |> L
                       let tail=  if tmpHolder <> None then tmpHolder.Value else []
                       Some(tail)
    | _ -> None 
    
let errorHandler = function
     | None -> "Invalid Program Passed"
     | Some x -> "Program is valid"

let runner =
     let program1 = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;END;ELSE;PRINT;ID;EOF]
     let program2 = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
     let program3 = [IF;ID;THEN;BEGIN;PRINT;ID;SEMICOLON;PRINT;ID;SEMICOLON;END;ELSE;PRINT;ID;EOF]
     printf "Is program: %A valid? "program1
     program1 |> S |> errorHandler |> printf "%s \n"
     printf "Is program: %A valid? "program2
     program2 |> S |> errorHandler |> printf "%s \n"
     printf "Is program: %A valid? "program3
     program3 |> S |> errorHandler |> printf "%s \n"