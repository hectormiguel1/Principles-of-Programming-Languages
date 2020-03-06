module Set_Two.Question_13_1

open System.Linq

//  S -> if E then S else S | begin S L | print E
//  L -> end | ; S L
//   E -> i

type TERMINAL = IF | THEN | ELSE | BEGIN | END | PRINT | SEMICOLON | ID | EOF

 
//Change names to be more unique,
// Add ends and semicolon branches 
type 'a ParseTree =
    | Branch_ID of ID: 'a 
    | Print of ID: 'a ParseTree
    | Branch_If of E: 'a ParseTree * THEN :'a ParseTree * ELSE :'a ParseTree
    | Branch_Begin of S: 'a ParseTree * L: 'a ParseTree
    | Branch_End
    | Branch_Semicolon of S: 'a ParseTree * L: 'a ParseTree
    | Branch_EOF 
    
let E = function
    | [] -> None
    | ID::xs -> Some(Branch_ID(ID), xs)
    | _ -> None
let eat terminal = function
    | x::xs when x = terminal -> Some(xs)
    | _ -> None 
let rec S = function
    | [EOF] -> None
    | IF::xs -> let tmpHolder = xs |> E
                let (e_tree,tail) = if tmpHolder <> None then tmpHolder.Value else (Branch_End,[EOF])
                let tmpHolder = (tail |> eat THEN).Value |> S
                let (then_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Branch_End,[EOF])
                let tmpHolder = (tail |> eat ELSE).Value |> S
                let (else_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Branch_End,[EOF])
                Some(Branch_If(e_tree,then_tree,else_tree),tail)
    | BEGIN::xs -> let tmpValue = xs |> S
                   let (s_tree, tail) = if tmpValue <> None then tmpValue.Value else (Branch_End, [EOF])
                   let tmpValue = tail |> L
                   let (l_tree, tail) = if tmpValue <> None then tmpValue.Value else (Branch_End, [EOF])
                   Some(Branch_Begin(s_tree, l_tree), tail)
    | PRINT::xs -> let tmpHolder = xs |> E
                   let (e_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Branch_End,[EOF])
                   Some(Print(e_tree), tail)
    | _ -> None 
and L = function
    | END::xs -> Some(Branch_End, xs)
    | SEMICOLON::xs -> let tmpHolder = xs |> S
                       let (s_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Branch_End, [EOF])
                       let tmpHolder = tail |> L
                       let (l_tree, tail) =  if tmpHolder <> None then tmpHolder.Value else (Branch_End, [EOF])
                       Some(Branch_Semicolon(s_tree,l_tree), tail)
    | _ -> None 
    
let errorHandler = function
     | None -> "Invalid Program Passed"
     | Some x -> match x with
                 | (tree,remainer) -> sprintf "Resulting Tree: %A" tree
let runner =
     let program1 = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
     program1 |> S |> errorHandler |> printf "%s \n"
