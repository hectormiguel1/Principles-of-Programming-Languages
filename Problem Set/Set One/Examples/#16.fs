module SetOne.Examples._16

 let rec gcd (a,b) =
    match (a,b) with 
    | (a,0) -> a
    | (a,b) -> gcd (b, (a % b))
    

 let rec simplify (x,y) =
     let factor = gcd (x,y)
     let areFactors = (x % factor = 0) , (y % factor = 0)
     match areFactors with
     | (true, true) when factor > 1-> simplify ((x/factor),(y/factor))
     | (_,_) -> (x,y)
    
 let (.+) (a,b) (c,d) = 
    let (e,f) = (((a*d) + (c*b)),(b*d))
    simplify (e,f)
    
    
 let (.*) (a,b) (c,d) =
   let (e,f) = (b*c),(a*d)
   simplify (e,f)    
 let runner =
     let a  = (999,621)
     let b = (211,901)
     let c = a .+ b
     printfn "Adding %A and %A is %A" a b c
     printfn "GCF of %A is %d" c (gcd c)