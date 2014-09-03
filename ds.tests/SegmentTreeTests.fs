namespace ds.Tests

open ds
open System
open NUnit.Framework

[<TestFixture>]
type SegmentTreeTests() = 
    let maxTree (values : int32 seq) = SegmentTree.OfList values max Int32.MinValue
    
    [<Test>]
    member __.EmptyTree() = Assert.That(SegmentTree.Query (maxTree []) (1, 1), Is.EqualTo Int32.MinValue)
    
    [<Test>]
    member __.NonexistentRange() = 
        Assert.That(SegmentTree.Query (maxTree [ 1; 2; 3 ]) (4, 5), Is.EqualTo Int32.MinValue)
    
    [<TestCase(0, 4, ExpectedResult = 5)>]
    [<TestCase(0, 0, ExpectedResult = 5)>]
    [<TestCase(1, 2, ExpectedResult = 4)>]
    [<TestCase(1, 3, ExpectedResult = 4)>]
    [<TestCase(3, 4, ExpectedResult = 1)>]
    [<TestCase(4, 4, ExpectedResult = -1)>]
    member __.MaxTree(left, right) = 
        let tree = maxTree [ 5; 3; 4; 1; -1 ]
        SegmentTree.Query tree (left, right)
