module Set_Two.Question_13_2

type tokens = ID | ADD | SUB | MUL | DIV | LPAREN | RPAREN | EOF

type 'a parseTree =
    | Br_Id 
    | Br_Add of E_branch: 'a parseTree * T_branch: 'a parseTree
    | Br_Sub of E_branch: 'a parseTree * T_branch: 'a parseTree
    | Br_Mul of T_branch: 'a parseTree * F_branch: 'a parseTree
    | Br_Div of T_branch: 'a parseTree * F_branch: 'a parseTree
    | F_branch of tree: 'a parseTree
    | T_branch of tree: 'a parseTree
    | E_branch of tree: 'a parseTree
    
let eat terminal = function
    | terminal::xs -> Some(xs)
    | _ -> None     
let rec E = function
    | [] -> None 
    | [EOF] -> None 
    | ADD::xs -> let tmpHolder = xs |> E
                 let (e_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [EOF])
                 let tmpHolder = tail |> T
                 let (t_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [EOF])
                 Some(Br_Add(e_tree,t_tree), tail)
    | SUB::xs -> let tmpHolder = xs |> E
                 let (e_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [EOF])
                 let tmpHolder = tail |> T
                 let (t_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [EOF])
                 Some(Br_Sub(e_tree,t_tree), tail)
    | x::xs -> let tmpHolder = (x::xs) |> T
               let (t_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [])
               Some(t_tree, tail)
and T = function
    | MUL::xs -> let tmpHolder = xs |> E
                 let (t_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [EOF])
                 let tmpHolder = tail |> T
                 let (f_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [EOF])
                 Some(Br_Mul(t_tree,f_tree), tail)
    | DIV::xs -> let tmpHolder = xs |> E
                 let (t_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [EOF])
                 let tmpHolder = tail |> T
                 let (f_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [EOF])
                 Some(Br_Div(t_tree,f_tree), tail)
    | x::xs -> let tmpHolder =  (x::xs) |> F
               let (f_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [])
               Some(f_tree, tail)
    | _ -> None 
and F = function
    | ID::xs -> let tmpHolder = xs |> E
                let (e_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [])
                Some(e_tree, tail)
    | LPAREN::xs -> let tmpHolder = xs |> E
                    let (e_tree, tail) = if tmpHolder <> None then tmpHolder.Value else (Br_Id, [EOF])
                    let tmpHolder = tail |> eat RPAREN
                    let tail = if tmpHolder <> None then tmpHolder.Value else [EOF]
                    Some(e_tree, tail)
    | RPAREN::xs -> E xs
    | _ -> None 
let errorHandler = function
     | None -> "Invalid Program Passed"
     | Some x -> match x with
                    (tree, tail) ->  sprintf "Tree: %A" tree
    
let runner =
    let program1 = [ID;ADD;ID;ADD;ID;ADD;ID;EOF]
    let program2 = [ID;SUB;ID;MUL;ID;EOF]
    let program3 = [LPAREN;ID;SUB;ID;RPAREN;MUL;ID;EOF]
    printf "Is program: %A valid? "program1
   // program1 |> E |> errorHandler |> printf "%s \n"
    printf "Is program: %A valid? "program2
   // program2 |> E |> errorHandler |> printf "%s \n"
    printf "Is program: %A valid? "program3
    program3 |> E |> errorHandler |> printf "%s \n"