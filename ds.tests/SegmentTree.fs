namespace ds.tests

open System
open System.Diagnostics

[<DebuggerDisplay("{DebugDisplay}")>]
type Node<'T> = 
    Node of Node<'T> 
            * int
            * Node<'T> 
            * int
            * 'T
            | Empty
    
    override this.ToString() = 
        let SimpleString = function
                | Empty -> "Empty"
                | v -> "Node"
        
        match this with
            | Empty -> "Empty"
            | Node(left, leftLimit, right, rightLimit, value) -> sprintf "Left:%s; LeftLimit:%d; Right:%s; RightLimit:%d; Value:%s"
                                                                        (SimpleString left) leftLimit (SimpleString right) rightLimit (value.ToString())   

    member this.DebugDisplay = this.ToString()

type SegmentTree<'T> (vals:'T seq, f:'T->'T->'T, defaultValue:'T) =
     
    let pairs ls =
            let rec inner rem acc = 
                match rem with 
                | [] -> acc
                | _::[]-> raise (new Exception "List must have an even number of elements")
                | x::y::rest -> inner rest ((x,y)::acc)
            inner ls [] |> List.rev

    let BuildTree (vals:'T[]) f =
        let appendIfNeeded ls = if (List.length ls) % 2 = 0 then ls else (Empty::ls)
        let buildNode (l, r) = 
             match (l,r) with
             | (Empty, Empty) -> raise (new Exception "Shouldn't build node with two empty children")
             | (Empty, Node(_, leftLimit, _ , rightLimit, value)) -> Node (Empty, leftLimit, r, rightLimit, value)
             | (Node(_, leftLimit, _, rightLimit, value), Empty) -> Node (Empty, leftLimit, r, rightLimit, value)
             | (Node(_, leftLimit, _, _,leftValue), Node(_, _, _, rightLimit, rightValue)) -> 
                    Node(l, leftLimit, r, rightLimit, f [leftValue;rightValue] ) 
        let rec addLevel lower = 
            match lower with
                    | single::[] -> single
                    | v -> appendIfNeeded v |> pairs |> List.map buildNode |> addLevel
        let lowerLevel = Array.mapi (fun index item -> Node(Empty, index, Empty, index, item)) vals |> Array.toList
        addLevel (appendIfNeeded lowerLevel)

    let listFunc = List.fold f defaultValue
    let root = BuildTree (Array.ofSeq vals) listFunc
    
    let rec QueryNode tree ((lVal, rVal) as seg) = 
        match tree with
            | Empty -> []
            | Node(_, leftLimit, _, rightLimit, _) as node when lVal <= leftLimit && rVal >= rightLimit -> [node]
            | Node(left, _, right, _, _) -> match (left, right) with
                                                |(Empty, rightNode) -> QueryNode rightNode seg
                                                |(leftNode, Empty) -> QueryNode leftNode seg
                                                |(Node(_, _, _ ,leftRightLimit ,_), Node(_, rightLeftLimit, _, _, _)) ->
                                                         if rVal <= leftRightLimit then QueryNode left seg 
                                                         elif lVal >= rightLeftLimit then QueryNode right seg 
                                                         else (QueryNode left (lVal, leftRightLimit)) @ (QueryNode right (rightLeftLimit, rVal))
    member this.Query tree func seg =
        QueryNode tree seg 
        |> List.map (function | Empty -> defaultValue 
                              | Node(_, _, _, _, value)-> value) 
        |> func 