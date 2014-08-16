namespace ds

type Queue<'T> private (theWorking : 'T list, theFront : 'T list Lazy, fLength, theRear : 'T list, rLength) = 
    
    let ensureFront() = 
        match rLength > fLength with
        | true -> (theWorking, lazy (theFront.Force() @ (List.rev theRear)), fLength + rLength, [], 0)
        | false -> (theWorking, theFront, fLength, theRear, rLength)
    
    let ensureWorking ((working, front : 'T list Lazy, fl, rear, rl) as t) = 
        if List.isEmpty working then (front.Force(), lazy ([]), fl, rear, rl)
        else t
    
    let (working : 'T list, front : 'T list Lazy, frontLength, rear : 'T list, rearLength) = 
        ensureFront() |> ensureWorking
    
    member this.Dequeue() = 
        match (working) with
        | [] -> (None, this)
        | hd :: tl -> (Some(hd), new Queue<'T>(tl, front, frontLength - 1, rear, rearLength))
    
    member this.Enqueue value = new Queue<'T>(working, front, frontLength, value :: rear, rearLength + 1)
    new(vals : 'T list) = new Queue<'T>(vals, lazy ([]), List.length vals, [], 0)
    new() = Queue<'T>(List.empty<'T>)
