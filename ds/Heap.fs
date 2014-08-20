namespace ds

open System
open System.Diagnostics

[<DebuggerDisplay("{DebugDisplay}")>]
module Common = 
    let uncurry f = fun (x, y) -> f x y
    
    let DecimalAdd first second adder defaultValue = 
        let rec inner first second carry acc = 
            match (first, second) with
            | ([], []) -> List.rev(if carry = defaultValue then acc else carry :: acc)
            | ([], x :: xs) -> 
                let (carry, res) = adder x carry
                if carry <> defaultValue then inner [] xs carry (res::acc)
                else List.rev(res::acc) @ xs
            | (v, []) -> inner [] v carry acc
            | (f :: fs, s :: ss) -> 
                let (carryRes, res) = adder f s
                let (carry2, totalRes) = adder res carry
                let (_, totalCarry) = adder carryRes carry2
                inner fs ss totalCarry (totalRes :: acc)
        inner first second defaultValue []

type Node<'T when 'T : equality>(nodes : Node<'T> list, value : 'T) = 
    static let mutable maxId = 0
    
    static let nextId() = 
        maxId <- maxId + 1
        maxId
    
    let id = nextId()
    member this.Id = id
    member this.nodes = nodes
    member this.value = value
    member this.DebugDisplay = this.ToString()
    override this.ToString() = sprintf "Value: %s Nodes: %d" (this.value.ToString()) (List.length this.nodes)
    member this.Equals(node : Node<'T>) = this.value = node.value && this.nodes = node.nodes
    
    override this.Equals(obj : Object) = 
        match obj with
        | :? Node<'T> as obj -> this.Equals obj
        | _ -> false
    
    override this.GetHashCode() = 
        297 * this.value.GetHashCode() * (List.map (fun i -> i.GetHashCode()) nodes |> List.fold (*) 197)

type Heap<'T when 'T : equality> private (roots : Option<Node<'T>> list, size : int, comparer : 'T -> 'T -> bool) = 
    let newHeap (roots, size) = new Heap<'T>(roots, size, comparer)
    
    static let mergeHeaps (thisRoots, secondRoots, comparer) = 
        let appendLeft (node : Node<'T>) child = Node(child :: node.nodes, node.value)
        
        let mergeTrees (toAdd : Node<'T> Option) tree = 
            match (tree, toAdd) with
            | (v, None) -> (None, v)
            | (None, v) -> (None, v)
            | (Some(one), Some(two)) -> 
                (Some(if comparer one.value two.value then appendLeft one two
                      else appendLeft two one), None)
        Common.DecimalAdd thisRoots secondRoots mergeTrees None
    
    let mergeInternal otherRoots = mergeHeaps (roots, otherRoots, comparer)
    
    let getHeadNode() = 
        let findMin (x : Node<'T>) (y : Node<'T>) = 
            if comparer x.value y.value then x
            else y
        
        let notEmpty = List.filter Option.isSome roots |> List.map Option.get
        match notEmpty with
        | [] -> None
        | hd :: tl -> Some(Seq.fold findMin hd tl)
    
    let head = getHeadNode()
    let headNode = Option.bind (fun (v : Node<'T>) -> Some(v.value)) head
    member this.Size : int = size
    member this.Roots : Option<Node<'T>> list = roots
    member this.Comparer = comparer
    member this.Merge(other : Heap<'T>) = newHeap (mergeInternal other.Roots, size + other.Size)
    member this.Add value = newHeap (mergeInternal [ Some(Node([], value)) ], size + 1)
    member this.HeadNode = head
    member this.Head = headNode
    
    member this.Dequeue() = 
        let compareNodes (v : Node<'T> option) (v2 : Node<'T> option) = 
            match (v, v2) with
            | (Some(v), Some(v2)) -> v.Id = v2.Id
            | _ -> false
        match this.HeadNode with
        | None -> (None, this)
        | Some(v) -> 
            let remaining = List.map (fun i -> if compareNodes this.HeadNode i then None else i) this.Roots
            let options = List.map (fun i -> Some(i)) v.nodes
            (Some(v.value), newHeap (mergeHeaps (remaining, options, comparer), size - 1))
    
    member this.Equals(otherHeap : Heap<'T>) = 
        if this.Roots.Length <> otherHeap.Roots.Length then false
        else 
            List.zip this.Roots otherHeap.Roots
            |> List.map (Common.uncurry (=))
            |> List.fold (&&) true
    
    override this.Equals(other : Object) = 
        match other with
        | :? Heap<'T> as otherHeap -> this.Equals otherHeap
        | _ -> false
    
    override this.GetHashCode() : int = List.map (fun x -> x.GetHashCode()) this.Roots |> List.fold (*) 397
    new(comparer) = new Heap<'T>([], 0, comparer)

module Heap = 
    let Roots(heap : Heap<'T>) = heap.Roots
    let Size(heap : Heap<'T>) = heap.Size
    let Add (heap : Heap<'T>) value = heap.Add value
    let Dequeue(heap : Heap<'T>) = heap.Dequeue()
    let Head(heap : Heap<'T>) = heap.Head
    let Merge (one : Heap<'T>) (two : Heap<'T>) = one.Merge two
    let OfList (values : 'T list) comparer = List.fold Add (new Heap<'T>(comparer)) values
