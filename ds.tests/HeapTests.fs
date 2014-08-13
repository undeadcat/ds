namespace ds.Tests

open ds
open System
open NUnit.Framework

[<TestFixture>]
type HeapTests() = 
    let minHeap (vals:'T list) = Heap.OfList vals (<)
    let mergeRoots =  Common.uncurry Heap.Merge >> Heap.Roots
    let Node (children, value) = Some({nodes = List.filter Option.isSome children |> List.map Option.get; value = value})

    [<Test>]
    member this.MergeEmpty() = 
        let empty = minHeap []
        let one = minHeap [1]
        Assert.That(empty.Merge empty, Is.EqualTo empty)
        Assert.That(one.Merge empty, Is.EqualTo one)
        Assert.That(one.Merge empty, Is.EqualTo one)
        Assert.That(empty.Merge one, Is.EqualTo one)

    [<Test>]
    member this.MergeSimple() = 
        let one = minHeap [1]
        let two = minHeap [2]
        let expected = [Node([Node([], 2)],1); None]
        let actual = mergeRoots (one, two)
        Assert.That (actual, Is.EqualTo expected)
    
    [<Test>]
    member this.Merge_AddAndCarry() = 
        let one = minHeap [1; 2; 3] //(1,2), (3)
        let two = minHeap [4; 5; 6] //(4,5), (6)
        let expected = [Node([Node([Node([],5)], 4); Node([], 2)],1); Node([Node([],6)],3); None] 
        Assert.That (mergeRoots (one, two), Is.EqualTo expected)
    
    [<Test>]
    member this.Merge_UnequalLengths() =
        let one = minHeap [1] //(1)
        let two = minHeap [2;3;4;] //(2,3), (4)
        //  1
        //2 4
        //3
        let expected = [Node([Node([Node([],3)],2); Node([],4)],1); None; None]
        Assert.That (mergeRoots(one, two), Is.EqualTo expected)
    
    [<Test>]
    member this.AddItems_FindMin() = 
        let vals = [5;4;3;6;10;7;8;9;2;1]
        let heap = minHeap []
        Assert.That(heap.Head, Is.EqualTo None)
        let res = List.scan Heap.Add heap vals
        let actual = res |> List.tail |> List.map (Heap.Head >> Option.get)
        Assert.That(actual, Is.EqualTo [5;4;3;3;3;3;3;3;2;1])
    
    [<Test>]
    member this.DequeueItems() =
        let vals = [10..-1..1]
        let mutable heap = minHeap vals
        for i in [1..10] do
            Assert.That(Heap.Head heap, Is.EqualTo(Some(i)))
            let (dequeued, result) = Heap.Dequeue heap
            Assert.That(dequeued, Is.EqualTo (Some(i)))
            heap <- result
        Assert.That (Heap.Dequeue heap |> fst, Is.EqualTo None)
            
