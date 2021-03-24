module DiskSpaceUsage.BalancedTreeView

open Avalonia.Media
open DiskSpaceUsage.BalancedTree
open DiskSpaceUsage.DiskItem

module private Backgrounds =
    let private all = [ "#724B5E"; "#9596A6"; "#316FA5"; "#263440"; "#F5ECDB" ]
    let mutable private cursor = 0

    let reset () =
        cursor <- 0

    let next () =
        let next = List.item cursor all
        let total = List.length all
        cursor <- (cursor + 1) % total
        next

[<RequireQualifiedAccess>]
module rec BalancedTreeView =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type Size =
        { width: double
          height: double }

    let private toLeaf diskItem =
        diskItem
        |> DiskItem.sizeInBytes
        |> Option.map (fun size -> { data = diskItem; weight = size })

    let private createTree (tree: TreeNode<DiskItem>) (size: Size): IView list =
        match tree with
        | LeafNode leaf ->
            let textHeight = 15.0

            [ TextBlock.create [ Canvas.top 2.0
                                 Canvas.left 2.0
                                 TextBlock.width (size.width - 4.0)
                                 TextBlock.height textHeight
                                 TextBlock.textTrimming TextTrimming.CharacterEllipsis
                                 TextBlock.text leaf.data.name ]
              Canvas.create [ Canvas.background (Backgrounds.next ())
                              Canvas.top (textHeight + 4.0)
                              Canvas.left 2.0
                              Canvas.width (size.width - 4.0)
                              Canvas.height (size.height - textHeight - 6.0) ]
            ]

        | BranchNode branch ->
            let leftWeight = BalancedTree.weight branch.left |> double
            let rightWeight = BalancedTree.weight branch.right |> double
            let leftRatio = leftWeight / (leftWeight + rightWeight)

            let leftSize, rightSize, rightTopOffset, rightLeftOffset =
                if size.width > size.height
                then
                    let leftWidth = size.width * leftRatio
                    ({ width = leftWidth; height = size.height },
                     { width = size.width - leftWidth; height = size.height },
                     0.0,
                     leftWidth)
                else
                    let leftHeight = size.height * leftRatio
                    ({ width = size.width; height = leftHeight },
                     { width = size.width; height = size.height - leftHeight },
                     leftHeight,
                     0.0)

            [ Canvas.create [
                  Canvas.top 0.0
                  Canvas.left 0.0
                  Canvas.width leftSize.width
                  Canvas.height leftSize.height
                  Canvas.children (tryCreateTree branch.left leftSize)
              ]
              Canvas.create [
                  Canvas.top rightTopOffset
                  Canvas.left rightLeftOffset
                  Canvas.width rightSize.width
                  Canvas.height rightSize.height
                  Canvas.children (tryCreateTree branch.right rightSize)
              ]
            ]

    let private minSize =
        { width = 75.0
          height = 50.0 }

    let private tryCreateTree (tree: TreeNode<DiskItem>) (size: Size): IView list =
        if size.width >= minSize.width && size.height >= minSize.height
            then createTree tree size
            else
                let view =
                    Canvas.create [ Canvas.background (Backgrounds.next ())
                                    Canvas.top 2.0
                                    Canvas.left 2.0
                                    Canvas.width (size.width - 4.0)
                                    Canvas.height (size.height - 4.0) ]
                [ view ]

    let create (children: DiskItem list) initialSize attrs: IView =
        Backgrounds.reset ()

        let leaves =
            children
            |> List.choose toLeaf
            |> List.filter (fun leaf -> leaf.weight > int64 0)

        let childViews =
            leaves
            |> BalancedTree.create
            |> Option.map BalancedTree.root
            |> Option.map (fun root -> tryCreateTree root initialSize)
            |> Option.defaultValue []

        let defaults = [
            Canvas.width initialSize.width
            Canvas.height initialSize.height
            Canvas.children childViews
        ]

        Canvas.create (defaults @ attrs) :> IView
