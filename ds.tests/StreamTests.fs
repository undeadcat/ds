namespace ds.Tests
open ds
open System
open NUnit.Framework
open ds.Stream
open System.Linq

[<TestFixture>]
type StreamTests() = 
    let testConcat ls1 ls2 = 
        Stream.Concat (Stream.OfList ls1) (Stream.OfList ls2) |> Stream.ToList  

    [<Test>]
    member this.Simple() = 
        let str = lazy(Cons(1, lazy(Cons(2, lazy(Nil)))))
        Assert.That(Stream.ToList str, Is.EqualTo [1;2])
    
    [<Test>]
    member this.ConcatNonEmpty() = 
        let s1 = Stream.OfList [1;2;3;4]
        let s2 = Stream.OfList [5;6;7;8]
        Assert.That(Stream.Concat s1 s2 |>Stream.ToList, Is.EqualTo(List.init 8 (fun i-> i+1)))
    
    [<Test>]
    member this.ConcatEmpty() =
        Assert.That (testConcat [] [], Is.EqualTo [])
        Assert.That (testConcat [1] [], Is.EqualTo [1])
        Assert.That (testConcat [] [1], Is.EqualTo [1])

    [<Test>]
    member this.Reverse() = 
        let s1 = Stream.OfList [1;2;3;4]
        let reversed = Stream.Reverse s1
        Assert.That(reversed |> Stream.ToList, Is.EqualTo([4;3;2;1]))
    
    [<Test>]
    member this.ReverseEmpty() = 
        let res = Stream.Empty<int>() |> Stream.Reverse |> Stream.ToList
        Assert.That(res, Is.EqualTo [])
    
