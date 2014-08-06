
namespace ds.Tests
open ds
open System
open NUnit.Framework
open System.Diagnostics

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

      
    [<Test>]
    member this.LoadTest() =

        let doTest q queuer dequeuer title iters  =
            let folder q i = 
                 queuer q i |> dequeuer 
            let sw = Stopwatch.StartNew()
            let nums = [1..iters]
            List.fold folder q nums|> ignore
            sw.Stop()
            printfn "%s %d %f" title iters (float sw.ElapsedTicks/float iters) 
        let tests=[1000; 10000; 50000;]
        let immutableTester = doTest (new Queue<int>()) (fun q i -> q.Enqueue i) (fun q-> q.Dequeue()|> snd ) "Immutable"
        let mutableTester = doTest (new System.Collections.Generic.Queue<int>(List.max tests)) 
                                (fun q i -> q.Enqueue i;q;) 
                                (fun q -> ignore (q.Dequeue());q;) 
                                "Mutable"
        let stackTester = doTest [] 
                                    (fun q i -> i::q) 
                                    (fun q -> match q with
                                                |[] -> raise (new Exception "Stack is empty")
                                                | x::xs->xs )
                                    "Stack"
        List.iter (fun x -> immutableTester x; mutableTester x; stackTester x) tests
         

   