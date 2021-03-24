module DiskSpaceUsageTests.BalancedTree

open NUnit.Framework
open FsUnit

open DiskSpaceUsage.BalancedTree

let private leaf name size = { name = name; size = int64 size }
let private leafNode name size = LeafNode (leaf name size)
let private branchNode left right = BranchNode { left = left; right = right }

[<Test>]
let ``building a simple balanced tree`` () =
    let leaves: Leaf list = [
        leaf "File A" 9
        leaf "File C" 11
        leaf "File B" 10
        leaf "File E" 30
        leaf "File D" 15
    ]

    let tree = BalancedTree.create leaves

    let expectedTree =
        branchNode
            (leafNode "File E" 30)
            (branchNode
                (branchNode
                     (leafNode "File A" 9)
                     (leafNode "File B" 10)
                )
                (branchNode
                    (leafNode "File C" 11)
                    (leafNode "File D" 15)
                )
            )

    tree |> Option.map BalancedTree.root |> should equal (Some expectedTree)
