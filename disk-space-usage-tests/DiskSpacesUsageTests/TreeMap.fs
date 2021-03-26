module DiskSpaceUsageTests.TreeMap

open NUnit.Framework
open FsUnit

open DiskSpaceUsage.TreeMap

let private leaf data weight = { data = data; weight = int64 weight }
let private leafNode data weight = Leaf (leaf data weight)
let private branchNode left right = Branch { left = left; right = right }

[<Test>]
let ``building a simple tree map`` () =
    let leaves: Leaf<string> list = [
        leaf "File A" 9
        leaf "File C" 11
        leaf "File B" 10
        leaf "File E" 30
        leaf "File D" 15
    ]

    let tree = TreeMap.create leaves

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

    tree |> Option.map TreeMap.root |> should equal (Some expectedTree)

[<Test>]
let ``building a tree map with a single leaf`` () =
    let leaves = [ leaf "File A" 9 ]

    let tree = TreeMap.create leaves

    let expectedTree = (leafNode "File A" 9)

    tree |> Option.map TreeMap.root |> should equal (Some expectedTree)
