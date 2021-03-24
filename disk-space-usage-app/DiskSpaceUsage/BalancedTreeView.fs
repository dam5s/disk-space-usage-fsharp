module DiskSpaceUsage.BalancedTreeView

open Avalonia.Media
open DiskSpaceUsage.BalancedTree
open DiskSpaceUsage.DiskItem
open DiskSpaceUsage.SizeView

module private Backgrounds =
    //https://color.adobe.com/Ninsurance%20Blue%202%20(big%20mtn%20moon%20dark)-color-theme-10409293
    let private all = [ "#DD724B5E"; "#DD9596A6"; "#DD316FA5"; "#DD263440"; "#DDF5ECDB" ]
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

    type Size = { width: double; height: double }
    type Offset = { top: double; left: double }

    let private leafPadding = 2.0

    let private leafView (depth: int) leaf size: IView list =
        let labelOffset = { top = leafPadding; left = leafPadding }
        let insetSize = { width = size.width - leafPadding * 2.0; height = size.height - leafPadding * 2.0 }
        let textSize = { width = insetSize.width; height = 15.0 }
        let leafOffset = { top = textSize.height + leafPadding * 2.0; left = leafPadding }
        let leafSize = { width = insetSize.width; height = insetSize.height - textSize.height - leafPadding }

        let estimatedMaxSizeViewWidth = 50.0

        let labelView =
            TextBlock.create [ Canvas.top labelOffset.top
                               Canvas.left labelOffset.left
                               TextBlock.width (insetSize.width - estimatedMaxSizeViewWidth)
                               TextBlock.height textSize.height
                               TextBlock.textTrimming TextTrimming.CharacterEllipsis
                               TextBlock.text leaf.data.name ]

        let sizeView =
            TextBlock.create [ Canvas.top labelOffset.top
                               Canvas.left labelOffset.left
                               TextBlock.textAlignment TextAlignment.Right
                               TextBlock.width insetSize.width
                               TextBlock.height textSize.height
                               TextBlock.text (SizeView.text leaf.data.size) ]

        let rectangleView children =
            Canvas.create [ Canvas.background (Backgrounds.next ())
                            Canvas.top leafOffset.top
                            Canvas.left leafOffset.left
                            Canvas.width leafSize.width
                            Canvas.height leafSize.height
                            Canvas.children children ]

        let childrenViews =
            match leaf.data.itemType with
            | File -> []
            | Folder attrs ->
                if depth >= 2
                then []
                else
                    let childrenInset = 8.0
                    let childrenOffset = { top = childrenInset; left = childrenInset }
                    let childrenSize = { width = insetSize.width - childrenInset * 2.0
                                         height = insetSize.height - textSize.height - childrenInset * 2.0 }

                    [ createWithDepth (depth + 1) attrs.children childrenSize [
                        Canvas.top childrenOffset.top
                        Canvas.left childrenOffset.left
                    ] ]

        [ labelView; sizeView; rectangleView childrenViews ]

    let branchView depth (branch: Branch<DiskItem>) size: IView list =
        let leftWeight = BalancedTree.weight branch.left |> double
        let rightWeight = BalancedTree.weight branch.right |> double
        let leftRatio = leftWeight / (leftWeight + rightWeight)

        let leftSize, rightSize, rightOffset =
            if size.width > size.height
            then
                let leftWidth = size.width * leftRatio
                ({ width = leftWidth; height = size.height },
                 { width = size.width - leftWidth; height = size.height },
                 { top = 0.0; left = leftWidth })
            else
                let leftHeight = size.height * leftRatio
                ({ width = size.width; height = leftHeight },
                 { width = size.width; height = size.height - leftHeight },
                 { top = leftHeight; left = 0.0 })

        [ Canvas.create [
              Canvas.top 0.0
              Canvas.left 0.0
              Canvas.width leftSize.width
              Canvas.height leftSize.height
              Canvas.children (createTree depth branch.left leftSize) ]
          Canvas.create [
              Canvas.top rightOffset.top
              Canvas.left rightOffset.left
              Canvas.width rightSize.width
              Canvas.height rightSize.height
              Canvas.children (createTree depth branch.right rightSize) ] ]

    let private minSize = { width = 75.0; height = 25.0 }

    let private createTree (depth: int) (tree: TreeNode<DiskItem>) (size: Size): IView list =
        if size.width >= minSize.width && size.height >= minSize.height
            then
                match tree with
                | LeafNode leaf -> leafView depth leaf size
                | BranchNode branch -> branchView depth branch size
            else
                let view =
                    Canvas.create [ Canvas.background (Backgrounds.next ())
                                    Canvas.top leafPadding
                                    Canvas.left leafPadding
                                    Canvas.width (size.width - leafPadding * 2.0)
                                    Canvas.height (size.height - leafPadding * 2.0) ]
                [ view ]

    let private toLeaf diskItem =
        diskItem
        |> DiskItem.sizeInBytes
        |> Option.map (fun size -> { data = diskItem; weight = size })

    let createWithDepth depth children initialSize attrs: IView =
        let leaves =
            children
            |> List.choose toLeaf
            |> List.filter (fun leaf -> leaf.weight > int64 0)

        let childViews =
            leaves
            |> BalancedTree.create
            |> Option.map BalancedTree.root
            |> Option.map (fun root -> createTree depth root initialSize)
            |> Option.defaultValue []

        let defaults = [ Canvas.width initialSize.width
                         Canvas.height initialSize.height
                         Canvas.children childViews
                         Canvas.background "#111" ]

        Canvas.create (defaults @ attrs) :> IView

    let create (children: DiskItem list) initialSize attrs: IView =
        Backgrounds.reset ()
        createWithDepth 0 children initialSize attrs
