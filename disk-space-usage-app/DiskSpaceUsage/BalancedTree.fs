module DiskSpaceUsage.BalancedTree

type TreeNode =
    | LeafNode of Leaf
    | BranchNode of Branch
and Leaf =
    { name: string
      size: int64 }
and Branch =
    { left: TreeNode
      right: TreeNode }

type BalancedTree =
    private BalancedTree of root:TreeNode

[<RequireQualifiedAccess>]
module BalancedTree =

    let root (BalancedTree root) = root

    let rec private size tree =
        match tree with
        | LeafNode leaf -> leaf.size
        | BranchNode branch -> (size branch.left) + (size branch.right)

    let rec private leafCount tree =
        match tree with
        | LeafNode _ -> 1
        | BranchNode branch -> (leafCount branch.left) + (leafCount branch.right)

    let create (leaves: Leaf list) =
        let mutable sortedLeaves: TreeNode list =
            leaves
            |> List.sortBy (fun l -> l.size)
            |> List.map LeafNode

        let mutable sortedTrees: TreeNode list = []

        let finished () =
            match sortedTrees with
            | [tree] -> leafCount tree = List.length leaves
            | _ -> false

        let findLightestTree (): TreeNode option =
            match sortedLeaves, sortedTrees with
            | leaf :: remainingLeaves, tree :: remainingTrees ->
                if size leaf < size tree
                then
                    sortedLeaves <- remainingLeaves
                    Some leaf
                else
                    sortedTrees <- remainingTrees
                    Some tree

            | leaf :: remainingLeaves, [] ->
                sortedLeaves <- remainingLeaves
                Some leaf

            | [], tree :: remainingTrees ->
                sortedTrees <- remainingTrees
                Some tree

            | [], [] ->
                None

        let mutable left: TreeNode option = None

        while not (finished ()) do
            let lightestTree = findLightestTree ()

            match left, lightestTree with
            | None, Some _ ->
                left <- lightestTree
            | Some leftTree, Some rightTree ->
                left <- None
                sortedTrees <- sortedTrees @ [
                    BranchNode { left = leftTree; right = rightTree }
                ]
            | _ -> ()

        sortedTrees
        |> List.tryHead
        |> Option.map BalancedTree
