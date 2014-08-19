namespace ds.Tests

open ds
open NUnit.Framework
open System.Collections.Generic

[<TestFixture>]
type TreeTests() = 
    let minTree vals = Tree.OfList Comparer.Default vals
    
    [<Test>]
    member this.Simple1() = 
        let tree = minTree [ 1; 2 ]
        Assert.That(tree, Iz.RedBlackTree<int>())
    
    [<Test>]
    member this.Simple2() = 
        let tree = minTree [ 1..10 ]
        Assert.That(tree, Iz.RedBlackTree<int>())
