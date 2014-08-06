
namespace ds
open System
open ds.Stream

type Queue<'T> private (theFront:'T stream, fLength, theRear:'T stream, rLength) =
    let (front:'T stream, frontLength, rear:'T stream, rearLength) = 
        match rLength > fLength with
            |true -> (Stream.Concat theFront (Stream.Reverse theRear), fLength + rLength, Stream.Empty<'T>(), 0)
            |false -> (theFront, fLength, theRear, rLength)

    member this.Dequeue() = 
        match (front.Force()) with
            | Nil -> (None, this)
            | Cons(hd, tl) -> (Some(hd), new Queue<'T>(tl, frontLength-1, rear, rearLength))

    member this.Enqueue value = 
        new Queue<'T>(front, frontLength, lazy(Cons(value, rear)), rearLength+1)

    new (vals:'T list) = 
        new Queue<'T>(Stream.OfList vals, 
            List.length vals, 
            Stream.Empty<'T>(), 0)
    new() = Queue<'T>(List.empty<'T>)

    