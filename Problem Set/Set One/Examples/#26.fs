let rec merge = function
    	    | ([], ys)       -> ys
    	    | (xs, [])       -> xs
    	    | (x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys)
                else y :: merge (x::xs, ys)
    
    	    let rec split = function
    	    | []       -> ([], [])
    	    | [a]      -> ([a], [])
    	    | a::b::cs -> let (M,N) = split cs
                (a::M, b::N)
    
    	    let rec mergesort = function
    	    | []  -> []
    	    | L   -> let (M, N) = split L
                merge (mergesort M, mergesort N)