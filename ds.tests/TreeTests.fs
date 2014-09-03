namespace ds.Tests

open ds
open NUnit.Framework
open System.Collections.Generic
open System

[<TestFixture>]
type TreeTests() = 
    let minTree vals = Tree.OfList Comparer.Default vals
    let getValue = 
        function 
        | Leaf -> raise (new Exception "Attempted to get value for leaf")
        | Node({ Value = v }) -> v

    [<Test>]
    member this.Simple1() = 
        let vals = [ 1; 2 ]
        let tree = minTree vals
        List.iter (Tree.Contains tree >> Assert.IsTrue) vals
        Assert.That(tree, Iz.RedBlackTree<int>())
        Assert.That(Tree.ListNodes tree |>List.map getValue, Is.EqualTo (vals))
    
    [<Test>]
    member this.Simple2() = 
        let vals = [ 10..-1..1 ]
        let tree = minTree vals
        List.iter (Tree.Contains tree >> Assert.IsTrue) vals
        Assert.That(tree, Iz.RedBlackTree<int>())
        Assert.That(Tree.ListNodes tree |>List.map getValue, Is.EqualTo (List.rev vals))
    
    [<Test>]
    member this.AccessByIndex() = 
        let vals = [ 0..10 ]
        let tree = minTree vals
        
        let actual = List.map (Tree.ByIndex tree
                      >> Option.get
                      >> getValue) vals
        Assert.That(actual, Is.EqualTo vals)
