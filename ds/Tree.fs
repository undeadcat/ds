namespace ds

open System
open System.Collections.Generic

type Color = 
    | Red
    | Black

type RBNode<'T> = 
    | Node of NodeData<'T>
    | Leaf

and NodeData<'T> = 
    { Left : RBNode<'T>
      Color : Color
      Value:'T
      Size : int
      Right : RBNode<'T> } 


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
    let private getSize = 
        function 
        | Leaf -> 0
        | Node({ Size = size }) -> size
    let private newTree (tree : RBTree<'T>) newRoot = RBTree(newRoot, tree.Comparer)
    
    let Find (tree : RBTree<'T>) (value : 'T) = 
        let rec inner = 
            function 
            | Leaf -> None
            | Node({Left=l; Value=v; Right=r}) as node -> 
                match tree.Comparer value v with
                | GT -> inner r
                | EQ -> Some(node)
                | LT -> inner l
        inner tree.Root

    let Contains (tree : RBTree<'T>) (value : 'T) = 
        match Find tree value with
        | None -> false
        | _ -> true
    
    let Add (tree : RBTree<'T>) (value : 'T) = 
        let balance = 
            function 
            | (Node({Left = Node({Left = a; 
                                Color = Red; 
                                Value = x;  
                                Right = b}); Color = Red; Value = y; Right = c}), Black, z, d)
            | (Node({Left = a; Color = Red; Value = x; Right = Node({Left = b; 
                                                        Color = Red; 
                                                        Value = y;  
                                                        Right=c})}), Black, z, d)
            | (a, Black, x, Node({Left = Node({Left = b; 
                                                     Color = Red; 
                                                     Value = y;  
                                                     Right=c}); Color = Red; Value = z; Right = d}))
            | (a, Black, x, Node({Left = b; Color = Red; Value = y; Right = Node({Left = c;
                                                        Color = Red; 
                                                        Value = z;  
                                                        Right=d})})) -> 
                let newR = Node({Left = c; 
                                   Color =  Black; 
                                   Size = getSize(c) + getSize(d) + 1;
                                   Value = z; 
                                   Right = d})
                let newL = Node({Left = a; 
                                   Size = getSize(a) + getSize(b) + 1;
                                   Color = Black; 
                                   Value = x; 
                                   Right = b})
                Node({Left = newL; Size = getSize(newR) + getSize(newL) + 1; Color = Red; Value = y;  Right = newR})
            | (l, c, v, r) -> Node({Left = l; Color = c; Value = v; Right = r; Size = getSize(l) + getSize(r) + 1})
        let rec inner = 
            function 
            | Leaf -> 
                Node({ Left = Leaf; Color = Red; Size = 1; Value = value; Right = Leaf })
            | Node({ Left = l; Color = c; Value = v; Right = r }) as node -> 
                match tree.Comparer value v with
                | EQ -> node //TODO. update dictionary keys.  
                | LT -> balance ((inner l), c,  v, r)
                | GT -> balance (l, c, v, (inner r))
        
        let makeRootBlack = 
            function 
            | Leaf -> Leaf
            | Node({ Color = Black }) as node -> node
            | Node({ Left = l; Color = Red; Size = size; Value = v; Right = r }) -> 
                Node({ Left = l
                       Color = Black
                       Size = size
                       Value = v
                       Right = r })
        
        inner tree.Root
        |> makeRootBlack
        |> newTree tree
    
    let ByIndex (tree : RBTree<'T>) (index : int) = 
        let rec inner = 
            function 
            | (Leaf, _) -> None
            | (Node({ Left = left; Right = right }) as node, index) -> 
                let leftCount = getSize left
                if index = leftCount  then Some(node)
                elif index < leftCount then inner (left, index)
                else inner (right, index - leftCount - 1)
        inner (tree.Root, index)
    
    let ListNodes (tree : RBTree<'T>) = 
        let rec inner = function
            | Leaf ->[]
            | Node({Left = l; Right = r}) as node ->inner l @ [node] @ inner r
        inner tree.Root
    
    let OfList comparer vals = List.fold Add (new RBTree<'T>(comparer)) vals
    