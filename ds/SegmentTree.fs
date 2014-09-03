namespace ds

open System
open System.Diagnostics

[<DebuggerDisplay("{DebugDisplay}")>]
type STNode<'T> = 
    | Node of STNode<'T> * int * STNode<'T> * int * 'T
    | Empty
    
    override this.ToString() = 
        let SimpleString = 
            function 
            | Empty -> "Empty"
            | _ -> "Node"
        match this with
        | Empty -> "Empty"
        | Node(left, leftLimit, right, rightLimit, value) -> 
            sprintf "Left:%s; LeftLimit:%d; Right:%s; RightLimit:%d; Value:%s" (SimpleString left) leftLimit 
                (SimpleString right) rightLimit (value.ToString())
    
    member this.DebugDisplay = this.ToString()

type SegmentTree<'T>(root : STNode<'T>, defaultValue : 'T, listFunction : 'T list -> 'T) = 
    member __.ListFunction = listFunction
    member __.DefaultValue = defaultValue
    member __.Root = root

module SegmentTree = 
    let private BuildRoot (vals : 'T []) f = 
        let appendIfNeeded ls = 
            if (List.length ls) % 2 = 0 then ls
            else (Empty :: ls)
        
        let buildNode (l, r) = 
            match (l, r) with
            | (Empty, Empty) -> failwith "Dummy. This doesn't happen"
            | (Empty, Node(_, leftLimit, _, rightLimit, value)) -> Node(Empty, leftLimit, r, rightLimit, value)
            | (Node(_, leftLimit, _, rightLimit, value), Empty) -> Node(Empty, leftLimit, r, rightLimit, value)
            | (Node(_, leftLimit, _, _, leftValue), Node(_, _, _, rightLimit, rightValue)) -> 
                Node(l, leftLimit, r, rightLimit, f [ leftValue; rightValue ])
        
        let rec addLevel lower = 
            match lower with
            | [] -> Empty
            | single :: [] -> single
            | v -> 
                appendIfNeeded v
                |> Common.Pairs
                |> List.map buildNode
                |> addLevel
        
        Array.mapi (fun index item -> Node(Empty, index, Empty, index, item)) vals
        |> Array.toList
        |> appendIfNeeded
        |> addLevel
    
    let OfList values f defaultValue = 
        let listFunction = (List.fold f defaultValue)
        SegmentTree(BuildRoot (Array.ofSeq values) listFunction, defaultValue, listFunction)
    
    let rec private QueryNode node ((lVal, rVal) as seg) = 
        match node with
        | Empty -> []
        | Node(_, leftLimit, _, rightLimit, _) as node when lVal <= leftLimit && rVal >= rightLimit -> [ node ]
        | Node(left, _, right, _, _) -> 
            match (left, right) with
            | (Empty, rightNode) -> QueryNode rightNode seg
            | (leftNode, Empty) -> QueryNode leftNode seg
            | (Node(_, _, _, leftRightLimit, _), Node(_, rightLeftLimit, _, _, _)) -> 
                if rVal <= leftRightLimit then QueryNode left seg
                elif lVal >= rightLeftLimit then QueryNode right seg
                else (QueryNode left (lVal, leftRightLimit)) @ (QueryNode right (rightLeftLimit, rVal))
    
    let Query (tree : SegmentTree<_>) seg = 
        QueryNode (tree.Root) seg
        |> List.map (function 
               | Empty -> tree.DefaultValue
               | Node(_, _, _, _, value) -> value)
        |> tree.ListFunction
