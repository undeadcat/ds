namespace ds.Tests

module Iz = 
    open ds
    open NUnit.Framework.Constraints
    open System
    
    type ValidationResult = 
        { Result : bool
          Message : String option }
        static member Success = 
            { Result = true
              Message = None }
    
    type Validator2<'T>(func : 'T -> ValidationResult) = 
        member this.Validate((value : 'T)) = func (value)
        
        static member Return() = 
            Validator2(fun _ -> 
                { Result = true
                  Message = None })
        
        static member Bind (prev : Validator2<'T>) (func : 'T -> ValidationResult) = 
            let nextValidator x = 
                let result = prev.Validate x
                if not result.Result then result
                else func x
            Validator2(nextValidator)
        
        new(func : 'T -> bool, message : string) = 
            new Validator2<'T>(fun x -> 
            if func x then 
                { Result = true
                  Message = None }
            else 
                { Result = false
                  Message = Some(message) })
        
        static member (>==) (v : Validator2<'T>, func : 'T -> ValidationResult) = Validator2.Bind v func
        static member (>==) (v : Validator2<'T>, v2 : Validator2<'T>) = Validator2.Bind v v2.Validate
    
    type RedBlackTreeConstraint<'T>() = 
        inherit EqualConstraint()
        
        let isBlack (node : RBNode<'T>) = 
            match node with
            | Leaf -> true
            | Node(_, Black, _, _) -> true
            | _ -> false
        
        let getNodes (tree : RBTree<'T>) = 
            let rec inner node = 
                match node with
                | Leaf -> []
                | Node(l, _, _, r) as node -> inner l @ [ node ] @ inner r
            inner tree.Root
        
        let getPaths (tree : RBTree<'T>) = 
            let rec inner node path = 
                match node with
                | Leaf -> [ path ]
                | Node(l, _, _, r) as node -> inner l (node :: path) @ inner r (node :: path)
            inner tree.Root []
        
        let childrenBlack = 
            function 
            | Leaf -> false
            | Node(l, _, _, r) -> isBlack l && isBlack r
        
        let isRedBlack (tree : RBTree<'T>) = 
            Validator2((fun (x : RBTree<'T>) -> isBlack x.Root), "Root should be black") 
            >== Validator2((fun x -> 
                           getPaths x
                           |> List.map (List.filter isBlack >> List.length)
                           |> Seq.distinct
                           |> Seq.length = 1), "Paths to leaves should contain the same number of black nodes") 
            >== Validator2((fun x -> 
                           getNodes x
                           |> List.filter (isBlack >> not)
                           |> List.forall childrenBlack), "Red nodes should have two black children")
            |> fun x -> x.Validate(tree)
        
        let mutable result = ValidationResult.Success
        
        override this.Matches(actual : Object) = 
            result <- isRedBlack (downcast actual)
            result.Result
        
        override this.WriteMessageTo(writer : MessageWriter) = 
            match result.Message with
            | None -> ignore()
            | Some(v) -> writer.Write(v)
    
    let RedBlackTree<'T>() = RedBlackTreeConstraint<'T>()
