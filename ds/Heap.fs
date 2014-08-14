namespace ds

open System
open System.Diagnostics

[<DebuggerDisplay("{DebugDisplay}")>]
module Common = 
    let uncurry f = fun (x,y) -> f x y

type Node<'T when 'T:equality> = 
    {nodes:Node<'T> list; value:'T} 
    
    member this.DebugDisplay with get() = this.ToString()

    override this.ToString() =
         sprintf "Value: %s Nodes: %d" (this.value.ToString()) (List.length this.nodes)

type Heap<'T when 'T:equality> private (roots: Option<Node<'T>> list, size:int, comparer:'T -> 'T-> bool) =
    static let decimalAdd first second adder defaultValue=
        let rec inner first second carry acc = 
            match (first, second) with
            | ([], []) -> if carry = defaultValue then acc else (carry::acc)
            | ([], x::xs) -> let (carry, res) = adder x carry
                             inner [] xs carry (res::acc)
            | (x::xs, []) -> let (carry, res) = adder x carry
                             inner xs [] carry (res::acc)
            | (f::fs, s::ss) -> 
                                //TODO. this is shit.
                                let (carryRes, res) = adder f s
                                let (carry2, totalRes) = adder res carry
                                let (_, totalCarry) = adder carryRes carry2
                                inner fs ss totalCarry (totalRes::acc)
        inner (List.rev first) (List.rev second) defaultValue []
    
    let newHeap (roots, size) = 
          new Heap<'T>(roots, size, comparer)
    
    static let mergeHeaps (thisRoots, secondRoots, comparer) = 
        let appendLeft node child = {nodes = child::node.nodes; value = node.value}
        let mergeTrees toAdd tree = 
            match (tree, toAdd) with
                | (v, None) -> (None, v)
                | (None, v) -> (None, v)
                | (Some(one), Some(two)) -> (Some(if comparer one.value two.value then appendLeft one two else appendLeft two one), None)
        decimalAdd thisRoots secondRoots mergeTrees None
    
    let mergeInternal otherRoots = 
        mergeHeaps (roots, otherRoots, comparer)

    let headNode () =
        let findMin x y = if comparer x.value y.value then x else y
        let notEmpty = List.filter Option.isSome roots |> List.map Option.get
        match notEmpty with
            | []-> None
            | hd::tl -> Some(Seq.fold findMin hd tl)
    
    member this.Size : int = size
    member this.Roots : Option<Node<'T>> list = roots
    member this.Comparer = comparer

    member this.Merge (other:Heap<'T>) =
        newHeap(mergeInternal other.Roots, size + other.Size)

    member this.Add value =
        newHeap(mergeInternal [Some({nodes=[]; value=value})], size + 1)
    
    member this.HeadNode = headNode()
    member this.Head with get() = Option.bind (fun v -> Some(v.value)) this.HeadNode
         
    member this.Dequeue() = 
        match this.HeadNode with 
            | None -> (None, this)
            | Some({nodes = nodes; value = value}) as v -> 
                   let remaining = List.filter ((<>) v) this.Roots
                   let options = List.map (fun i -> Some(i)) nodes
                   (Some(value), newHeap(mergeHeaps(options, remaining, comparer), size - 1))

    member this.Equals (otherHeap:Heap<'T>) =
        if this.Roots.Length <> otherHeap.Roots.Length then false
        else 
            List.zip this.Roots otherHeap.Roots
             |> List.map (Common.uncurry (=))
             |> List.fold (&&) true
    
    override this.Equals (other:Object) = 
        match other with
            | :? Heap<'T> as otherHeap -> this.Equals otherHeap
            | _ -> false

    override this.GetHashCode() : int= 
        List.map (fun x -> x.GetHashCode()) this.Roots
            |> List.fold (*) 397
     
    new (comparer) =
       new Heap<'T>([], 0, comparer)

 module Heap = 
    let Roots (heap:Heap<'T>) = 
        heap.Roots
    let Size (heap:Heap<'T>) =
        heap.Size
    let Add (heap:Heap<'T>) value = 
        heap.Add value
    let Dequeue (heap:Heap<'T>) = 
        heap.Dequeue()
    let Head (heap:Heap<'T>) = 
        heap.Head
    let Merge (one:Heap<'T>) (two:Heap<'T>) = 
        one.Merge two
    let OfList (values:'T list) comparer = 
        List.fold Add (new Heap<'T>(comparer)) values
