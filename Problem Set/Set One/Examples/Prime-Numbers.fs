module SetOne.Prime_Numbers

(*
A prime number is a number whose only divisors are 1 and itself
To test weather a number is prime we can divide it by all primes before its closest sqrt
there we test 2, 5,7,11 so forth till floor (sqrt of number) prime number
we are gonna start the list with 2,5,7,11 and then compute the rest from that starting position.
if % 2 or 5 or 7 or 11... so forth, number is not prime 
*)

let IsPrimeMultipleTest n x =
   x = n || x % n <> 0

let rec RemoveAllMultiples listn listx =
   match listn with
   | head :: tail -> RemoveAllMultiples tail (List.filter (IsPrimeMultipleTest head) listx)
   | [] -> listx


let GetPrimesUpTo n =
    let max = int (sqrt (float n))
    RemoveAllMultiples [ 2 .. max ] [ 2 .. n ]

printfn "Primes Up To %d:\n %A" 100 (GetPrimesUpTo 100)
            
