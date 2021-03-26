module DiskSpaceUsage.TreeMap

type BinaryTree<'a> =
    | Leaf of Leaf<'a>
    | Branch of Branch<'a>
and Leaf<'a> =
    { data: 'a
      weight: int64 }
and Branch<'a> =
    { left: BinaryTree<'a>
      right: BinaryTree<'a> }

type TreeMap<'a> =
    private TreeMap of root:BinaryTree<'a>

[<RequireQualifiedAccess>]
module TreeMap =

    let root (TreeMap root) = root

    let rec weight tree =
        match tree with
        | Leaf leaf -> leaf.weight
        | Branch branch -> (weight branch.left) + (weight branch.right)

    let rec private leafCount tree =
        match tree with
        | Leaf _ -> 1
        | Branch branch -> (leafCount branch.left) + (leafCount branch.right)

    let create (leaves: Leaf<'a> list): TreeMap<'a> option =
        let mutable sortedLeaves: BinaryTree<'a> list =
            leaves
            |> List.sortBy (fun l -> l.weight)
            |> List.map Leaf

        let mutable sortedTrees: BinaryTree<'a> list = []

        let treeIsCompleted () =
            match sortedTrees with
            | [tree] -> leafCount tree = List.length leaves
            | _ -> false

        let takeLightestTree (): BinaryTree<'a> option =
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

        let mutable left: BinaryTree<'a> option = None

        while not (treeIsCompleted ()) do
            let lightestTree = takeLightestTree ()

            match left, lightestTree with
            | None, Some _ ->
                left <- lightestTree
            | Some leftTree, Some rightTree ->
                left <- None
                sortedTrees <- sortedTrees @ [
                    Branch { left = leftTree; right = rightTree }
                ]
            | Some leftTree, None ->
                left <- None
                sortedTrees <- sortedTrees @ [ leftTree ]
            | _ -> ()

        sortedTrees
        |> List.tryHead
        |> Option.map TreeMap
