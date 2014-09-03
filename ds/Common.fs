namespace ds

module Common = 
    open System
    
    let Uncurry f = fun (x, y) -> f x y
    let Curry f = fun x y -> f (x, y)
    
    let Pairs ls = 
        let rec inner rem acc = 
            match rem with
            | [] -> acc
            | _ :: [] -> raise (new Exception "List must have an even number of elements")
            | x :: y :: rest -> inner rest ((x, y) :: acc)
        inner ls [] |> List.rev
    
    let DecimalAdd first second adder defaultValue = 
        let rec inner first second carry acc = 
            match (first, second) with
            | ([], []) -> 
                List.rev (if carry = defaultValue then acc
                          else carry :: acc)
            | ([], x :: xs) -> 
                let (carry, res) = adder x carry
                if carry <> defaultValue then inner [] xs carry (res :: acc)
                else List.rev (res :: acc) @ xs
            | (v, []) -> inner [] v carry acc
            | (f :: fs, s :: ss) -> 
                let (carryRes, res) = adder f s
                let (carry2, totalRes) = adder res carry
                let (_, totalCarry) = adder carryRes carry2
                inner fs ss totalCarry (totalRes :: acc)
        inner first second defaultValue []
