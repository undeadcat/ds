namespace ds.Tests

open ds
open NUnit.Framework

[<TestFixture>]
type HeapTests() = 
    let minHeap (vals:'T list) = Heap.OfList vals (<)
    let mergeRoots =  Common.Uncurry Heap.Merge >> Heap.Roots >> List.rev
    let Node (children, value) = Some(Node(List.filter Option.isSome children |> List.map Option.get,value))

    let compareTrees (one:Heap<'T>) (two : Heap<'T>) = 
        if one.Roots.Length <> two.Roots.Length then false
        else 
            List.zip one.Roots two.Roots
            |> List.map (Common.Uncurry (=))
            |> List.fold (&&) true

    let dequeueAll heap =
        let rec inner (heap:Heap<'T>) acc = 
            let returnThis() = (List.rev acc, heap)
            match heap.Head with
            |None -> returnThis() 
            |_ -> match Heap.Dequeue heap with 
                    |(None, heap) -> returnThis() 
                    |(Some(deq), heap) -> inner heap (deq::acc) 
        inner heap []

    [<Test>]
    member __.MergeEmpty() = 
        let empty = minHeap []
        let one = minHeap [1]
        Assert.True(compareTrees(empty.Merge empty) empty)
        Assert.True(compareTrees(one.Merge empty) one)
        Assert.True(compareTrees(one.Merge empty) one)
        Assert.True(compareTrees(empty.Merge one) one)

    [<Test>]
    member __.MergeSimple() = 
        let one = minHeap [1]
        let two = minHeap [2]
        let expected = [Node([Node([], 2)],1); None]
        let actual = mergeRoots (one, two)
        Assert.That (actual, Is.EqualTo expected)
    
    [<Test>]
    member __.Merge_AddAndCarry() = 
        let one = minHeap [1; 2; 3] //(1,2), (3)
        let two = minHeap [4; 5; 6] //(4,5), (6)
        let expected = [Node([Node([Node([],5)], 4); Node([], 2)],1); Node([Node([],6)],3); None] 
        Assert.That (mergeRoots (one, two), Is.EqualTo expected)
    
    [<Test>]
    member __.Merge_UnequalLengths() =
        let one = minHeap [1] //(1)
        let two = minHeap [2;3;4;] //(2,3), (4)
        //  1
        //2 4
        //3
        let expected = [Node([Node([Node([],3)],2); Node([],4)],1); None; None]
        Assert.That (mergeRoots(one, two), Is.EqualTo expected)
    
    [<Test>]
    member __.AddItems_FindMin() = 
        let vals = [5;4;3;6;10;7;8;9;2;1]
        let heap = minHeap []
        Assert.That(heap.Head, Is.EqualTo None)
        let res = List.scan Heap.Add heap vals
        let actual = res |> List.tail |> List.map (Heap.Head >> Option.get)
        Assert.That(actual, Is.EqualTo [5;4;3;3;3;3;3;3;2;1])
    
    [<Test>]
    member __.DequeueItems() =
        let vals = [10..-1..1]
        let heap = minHeap vals
        let (res, heap) = dequeueAll heap 
        Assert.That (res, Is.EqualTo [1..10])
        Assert.That (heap.Head, Is.EqualTo None)
        Assert.That (heap.Dequeue() |> fst, Is.EqualTo (None))
    
    [<Test>]
    member __.SameItems() =
        let vals = [1;1;1;1;2;2;2;2;3;3;3]
        let heap = minHeap vals
        Assert.That(fst (dequeueAll heap), Is.EqualTo vals)

    [<Test>]
    member __.Size() =
        let heap = minHeap [1;2;3]
        Assert.That(Heap.Size heap, Is.EqualTo 3)
        Assert.That(Heap.Add heap 4|> Heap.Size, Is.EqualTo 4)
        Assert.That(Heap.Dequeue heap |> snd |> Heap.Size, Is.EqualTo 2)
        Assert.That(Heap.Merge (minHeap [5;6]) heap |> Heap.Size, Is.EqualTo 5)