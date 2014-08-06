namespace ds

open System

type 'a cell = Nil | Cons of 'a * 'a stream
and 'a stream = 'a cell Lazy


module Stream =
    let private BuildList ls = List.fold (fun stream i -> Cons(i, lazy(stream))) (Nil) ls

    let private Collect (s: 'a stream) =
        let rec inner (rem:'a stream) acc = 
                match rem.Force() with
                    | Nil -> acc
                    | Cons(hd, tl) -> inner (tl) (hd::acc)
        inner s []

    let OfList ls = lazy((List.rev >> BuildList) ls)

    let Empty<'T>() :'T stream = lazy(Nil)
            
    let rec Concat (s1:stream<'T>) (s2:stream<'T>) = 
        match (s1.Force(), s2.Force()) with
            | (Nil, _) -> s2
            | (_, Nil) -> s1
            | (Cons(hd1, tl1), _) -> lazy(Cons(hd1, Concat (tl1) s2))
    
    let rec Reverse (s1:stream<'T>) = lazy((Collect >> List.rev >> BuildList) s1)
    
    let ToList s1 = (Collect >> List.rev) s1

