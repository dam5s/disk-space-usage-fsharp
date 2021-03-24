module DiskSpaceUsage.BalancedTree

type TreeNode<'a> =
    | LeafNode of Leaf<'a>
    | BranchNode of Branch<'a>
and Leaf<'a> =
    { data: 'a
      weight: int64 }
and Branch<'a> =
    { left: TreeNode<'a>
      right: TreeNode<'a> }

type BalancedTree<'a> =
    private BalancedTree of root:TreeNode<'a>

[<RequireQualifiedAccess>]
module BalancedTree =

    let root (BalancedTree root) = root

    let rec private weight tree =
        match tree with
        | LeafNode leaf -> leaf.weight
        | BranchNode branch -> (weight branch.left) + (weight branch.right)

    let rec private leafCount tree =
        match tree with
        | LeafNode _ -> 1
        | BranchNode branch -> (leafCount branch.left) + (leafCount branch.right)

    let create (leaves: Leaf<'a> list) =
        let mutable sortedLeaves: TreeNode<'a> list =
            leaves
            |> List.sortBy (fun l -> l.weight)
            |> List.map LeafNode

        let mutable sortedTrees: TreeNode<'a> list = []

        let treeIsCompleted () =
            match sortedTrees with
            | [tree] -> leafCount tree = List.length leaves
            | _ -> false

        let takeLightestTree (): TreeNode<'a> option =
            match sortedLeaves, sortedTrees with
            | leaf :: remainingLeaves, tree :: remainingTrees ->
                if weight leaf < weight tree
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

        let mutable left: TreeNode<'a> option = None

        while not (treeIsCompleted ()) do
            let lightestTree = takeLightestTree ()

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
