module SetOne.Leap_Years

open System

(*
    A leap year is one that is divisible by 4
    not by 100
    but by 400
*)

let leapYearTest year =
    (year % 4 = 0 && year % 100 <> 0 ) || (year % 400 = 0)
           
let findLeapYears yearList max  =
    let leapYears = List.filter (leapYearTest) yearList
    let rec splitListAtNItem list item =
      match item with
       | 0 -> []
       | n ->
          match list with
          | [] -> []
          | head::tail -> head::splitListAtNItem tail (item - 1)
       
    splitListAtNItem leapYears max
    
    
let runner =
    let start = 2020
    let yearList = [start .. int(System.Int16.MaxValue)]
    printfn "Next leap years are:\n %A" (findLeapYears yearList 30) 