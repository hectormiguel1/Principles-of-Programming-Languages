module Set_Two.Question_11

type Student = {Name:string;Credits:int;GPA:float}

let runner =
        let student1 = {Name="James";Credits=109;GPA=3.84}
        printf "%A \n " student1
        