
namespace ds.Tests
open ds
open System
open NUnit.Framework

[<TestFixture>]
type QueueTests() = 
    
    let dump q = 
        let rec inner (q:Queue<'T>) acc =
            match q.Dequeue() with
            | (None, _) -> acc
            | (Some(v), q) -> inner q (v::acc)
        inner q [] |> List.rev

    [<Test>]
    member this.EmptyQueue() =
        let queue = new Queue<int>()
        Assert.That(queue.Dequeue() |>fst|> Option.isNone, Is.EqualTo(true))
    
    [<Test>]
    member this.SeqConstructor() = 
        let queue = new Queue<int>([1;2;3;4])
        let actual = dump queue
        Assert.That (actual, Is.EqualTo [1;2;3;4])
    
    [<Test>]
    member this.Enqueue() = 
        let ls = List.init 10 (fun i ->i )
        let queue = List.fold (fun (q:Queue<int>) i -> q.Enqueue i) (new Queue<int>()) ls
        Assert.That (dump queue, Is.EqualTo ls)
   