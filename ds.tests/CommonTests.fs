module CommonTests

open ds
open NUnit.Framework
open System
open System.Diagnostics

[<TestFixture>]
type CommonTests() = 
    
    let ToList a = 
        let rec inner num res = 
            let (div, rem) = Math.DivRem(num, 10)
            if div = 0 then 
                if rem <> 0 then rem :: res
                else res
            else inner div (rem :: res)
        inner a [] |> List.rev
    
    let OfList ls = List.rev ls |> List.fold (fun s i -> s * 10 + i) 0
    
    [<Test>]
    member this.ListFuncs() = 
        Assert.That(ToList 110, Is.EqualTo [ 0; 1; 1 ])
        Assert.That(ToList 123, Is.EqualTo [ 3; 2; 1 ])
        Assert.That(OfList [ 1; 2; 3 ], Is.EqualTo 321)
        Assert.That(OfList [ 0; 2; 2 ], Is.EqualTo 220)
    
    [<Test>]
    [<TestCase(1, 2, Result = 3, TestName = "Simple")>]
    [<TestCase(19, 2, Result = 21, TestName = "Carry")>]
    [<TestCase(999, 1, Result = 1000, TestName = "CarryMany")>]
    [<TestCase(11111, 1, Result = 11112, TestName = "EndsEarlier")>]
    [<TestCase(1, 11111, Result = 11112, TestName = "EndsEarlier2")>]
    [<TestCase(11, 999, Result = 1010, TestName = "EndsEarlierWithCarry")>]
    [<TestCase(55, 55, Result = 110, TestName = "CarryAtEnd")>]
    [<TestCase(66, 66, Result = 132, TestName = "CarryAndAdd")>]
    [<TestCase(110, 110, Result = 220, TestName = "Zeros")>]
    member this.TestAdd(a, b) = 
        let adder x y = Math.DivRem(x + y, 10)
        Common.DecimalAdd (ToList(a)) (ToList(b)) adder 0 |> OfList
    
    [<Test>]
    member this.Stupid() = 
        let one = [ 1..100000 ]
        let two = [ 1 ]
        let mutable res = [ 0 ]
        let sw = Stopwatch.StartNew()
        for i in 0..1000 do
            res <- two @ one
        sw.Stop()
        Console.WriteLine(sw.ElapsedMilliseconds)
