namespace ds

open System
open System.Collections.Generic

type Color = 
    | Red
    | Black

type RBNode<'T> = 
    | Node of RBNode<'T> * Color * 'T * RBNode<'T>
    | Leaf

type Comparison = 
    | LT
    | GT
    | EQ

type RBTree<'T>(root : RBNode<'T>, comparer : 'T -> 'T -> Comparison) = 
    member this.Comparer = comparer
    member this.Root = root
    new(comparer : IComparer<'T>) = 
        let comparerFunc x y = 
            match comparer.Compare(x, y) |> Math.Sign with
            | 1 -> GT
            | 0 -> EQ
            | _ -> LT
        new RBTree<'T>(Leaf, comparerFunc)

module Tree = 
    let private newTree (tree : RBTree<'T>) newRoot = RBTree(newRoot, tree.Comparer)
    
    let Find (tree : RBTree<'T>) (value : 'T) = 
        let rec inner = 
            function 
            | Leaf -> None
            | Node(l, _, v, r) as node -> 
                match tree.Comparer value v with
                | GT -> inner r
                | EQ -> Some(node)
                | LT -> inner l
        inner tree.Root
    
    let Add (tree : RBTree<'T>) (value : 'T) = 
        let balance = 
            function 
            | (Node(Node(a, Red, x, b), Red, y, c), _, z, d)
            | (Node(a, Red, x, Node(b, Red, y, c)), _, z, d)
            | (a, _, x, Node(b, Red, y, Node(c, Red, z, d)))
            | (a, _, x, Node(Node(b, Red, y, c), Red, z, d)) -> 
                Node(Node(a, Black, x, b), Red, y, Node(c, Black, z, d))
            | (l, c, v, r) -> Node(l, c, v, r)
        
        let rec inner = 
            function 
            | Leaf -> Node(Leaf, Red, value, Leaf)
            | Node(l, c, v, r) -> 
                match tree.Comparer v value with
                | EQ -> Node(l, c, value, r) //TODO. test  nodes equal by key, but not by value.  
                | LT -> balance ((inner l), c, value, r)
                | GT -> balance (l, c, value, (inner r))
        
        let makeRootBlack = function
             | Leaf -> Leaf
             | Node(l, c, v,r ) as node -> if c = Black then node else Node(l, Black, v, r)  

        inner tree.Root |> makeRootBlack |> newTree tree
    
    let OfList(comparer : IComparer<'T>) = List.fold Add (new RBTree<'T>(comparer))
