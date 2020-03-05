module Set_Two.Question_12

//Polymorphic tree 
type 'a Tree =
    | Leaf
    | Branch of ID: 'a * Left:'a Tree  * Right: 'a Tree
 
//Function used to transform the tree adding the left nodes of the tree to the right most leaf of the left of the tree
//Used when removing the root of the tree, transform the tree, instead of deleting all the subsequent nodes.    
let rec transformTree leftTree rightTree =
    match leftTree with
    | Leaf -> rightTree
    | Branch (id, left, right ) -> Branch(id, left, (transformTree right rightTree))
    
//Polymorphic remove function, removes ID from passed tree.   
let rec remove ID = function
    | Leaf -> None
    | Branch (id, Leaf, Leaf) when id = ID -> Some (Leaf)
    | Branch (id, left, right) when id = ID -> Some (transformTree left right)
    | Branch (id, leftTree, rightTree) -> if ID < id then let newLeftBranch = remove ID leftTree
                                                          if newLeftBranch <> None then Some (Branch (id, (remove ID leftTree).Value, rightTree))
                                                          else None
                                          else let newRightBranch = remove ID rightTree
                                               if newRightBranch <> None then Some (Branch(id, leftTree,(remove ID rightTree).Value))
                                               else None                                          
//Error Handler                                           
let errorHandler = function
    | None -> "Passed id could not be found in the tree"
    | Some x -> sprintf "Modified (not really) Tree: %A" x
    
//Runner Tester Code    
let runner =
    let tree = (Branch (23,(Branch(10,(Branch(9,Leaf,Leaf)),Leaf)),(Branch(50,(Branch(49,Leaf,Leaf)),Leaf))))
    printf "Current Tree %A \n" tree
    tree |> remove 10 |> errorHandler |> printf "%s\n "
    tree |> remove 50 |> errorHandler |> printf "%s\n"
    tree |> remove 23 |> errorHandler |> printf "%s\n"
    tree |> remove 9 |> errorHandler |> printf "%s\n"
    tree |> remove 49 |> errorHandler |> printf "%s\n"
    tree |> remove 21 |> errorHandler |> printf "%s\n"