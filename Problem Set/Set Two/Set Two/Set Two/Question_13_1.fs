module Set_Two.Question_13_1

open System.Linq

//  S -> if E then S else S | begin S L | print E
//  L -> end | ; S L
//   E -> i

type TERMINAL = IF | THEN | ELSE | BEGIN | END | PRINT | SEMICOLON | ID | EOF
type 'a ParseTree =
    | Leaf
    | Parent of Tree: 'a ParseTree
    | Print of ID:'a
    | If of E:TERMINAL * THEN :'a ParseTree * ELSE :'a ParseTree
    | Begin of S: 'a ParseTree * L: 'a ParseTree
     
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
     elif token = IF then
          match nextToken with
          | ID -> true
          | _ -> false
      else false 
 
let rec getInnerTokens tokens innerTokens =
     match tokens with
     | [] -> if List.isEmpty innerTokens then None else Some(innerTokens, tokens)
     | x::xs when x = END -> Some(innerTokens,xs) 
     | x::xs when x <> END -> getInnerTokens xs (innerTokens@[x])
                     
let rec SProduction tokens =
     match tokens with
     | [] -> Some(Leaf)
     | [x] when x = EOF -> Some(Leaf)
     | x::y::ys -> match x with
                    | IF -> let nextTokenMatch = matchNextTocken x y
                            if nextTokenMatch
                            then
                                 let innerTokens = (getInnerTokens ys [])
                                 let b::bs, remaining = if innerTokens <> None then innerTokens.Value else ([EOF],[EOF])
                                 let innerTokens2 = getInnerTokens remaining []
                                 let c::cs, remaining2 = if innerTokens2 <> None then innerTokens2.Value else ([EOF],[EOF])
                                 Some (If(y,(SProduction bs).Value,(SProduction cs).Value))
                            else None
                            
                    | BEGIN -> let innertokens = getInnerTokens ys []
                               let inner::inners, remaining = if innertokens <> None then innertokens.Value else ([EOF],[EOF])
                               Some (Begin((SProduction inners).Value, (LProduction remaining)))
                    | PRINT ->  if y = ID then Some(Print y) else None
                    | SEMICOLON -> Some((SProduction (y::ys)).Value)
and LProduction = function
     | [] -> Leaf
     | x::xs -> match x with
                | SEMICOLON -> (SProduction xs).Value
                   
 
let errorHandler = function
     | None -> "Invalid Program Passed"
     | Some x -> sprintf "Resulting Tree: %A" x
     
let runner =
     let program1 = [IF;ID;THEN;IF;ID;THEN;PRINT;ID;ELSE;PRINT;ID;ELSE;BEGIN;PRINT;ID;END;EOF]
     program1 |> SProduction |> errorHandler |> printf "%s\n"