module DiskSpaceUsage.BalancedTreeView

open Avalonia.Layout
open Avalonia.Media
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open BalancedTree
open DiskItem
open SizeView
open Styles

module private Backgrounds =
    let private all = [ "#844"; "#484"; "#448"; "#488"; "#848" ]
    let mutable private cursor = 0

    let reset () =
        cursor <- 0

    let next () =
        let next = List.item cursor all
        let total = List.length all
        cursor <- (cursor + 1) % total
        next

type Size = { width: double; height: double }
type Offset = { top: double; left: double }

[<RequireQualifiedAccess>]
module Canvas =
    let make offset size attrs =
        let defaults = [
            Canvas.left offset.left
            Canvas.top offset.top
            Canvas.width size.width
            Canvas.height size.height
        ]
        Canvas.create (defaults @ attrs)

[<RequireQualifiedAccess>]
module rec BalancedTreeView =

    type Config =
        { children: DiskItem list
          size: Size
          onItemSelected: DiskItem -> unit }

    let private noOffset = { top = 0.0; left = 0.0 }
    let private leafPadding = 2.0
    let private leafRectangleColor = "#6fff"

    let private leafView (depth: int) leaf (config: Config): IView list =
        let size = config.size
        let titleOffset = noOffset
        let titleSize = { width = size.width; height = 20.0 }
        let leafOffset = { top = titleSize.height; left = leafPadding }
        let leafSize = { width = size.width - leafPadding * 2.0; height = size.height - titleSize.height - leafPadding }

        let topBar =
            DockPanel.create [
                Canvas.top titleOffset.top
                Canvas.left titleOffset.left
                DockPanel.width titleSize.width
                DockPanel.height titleSize.height
                DockPanel.onTapped (fun _ -> config.onItemSelected leaf.data)
                DockPanel.classes ["topBar"]
                DockPanel.children [
                    TextBlock.create [ DockPanel.dock Dock.Right
                                       TextBlock.verticalAlignment VerticalAlignment.Center
                                       TextBlock.margin (4.0, 0.0)
                                       TextBlock.text (SizeView.text leaf.data.size) ]
                    TextBlock.create [ DockPanel.dock Dock.Left
                                       TextBlock.verticalAlignment VerticalAlignment.Center
                                       TextBlock.margin (4.0, 0.0)
                                       TextBlock.textTrimming TextTrimming.CharacterEllipsis
                                       TextBlock.text leaf.data.name ]
                ]
            ]

        let rectangleView children =
            Canvas.make leafOffset leafSize
                [ Canvas.background leafRectangleColor
                  Canvas.children children ]

        let childrenViews =
            match leaf.data.itemType with
            | File -> []
            | Folder attrs ->
                if depth >= 2
                then []
                else
                    let childrenInset = 16.0
                    let childrenOffset = { top = childrenInset; left = childrenInset }
                    let childrenSize = { width = leafSize.width - childrenInset * 2.0
                                         height = leafSize.height - childrenInset * 2.0 }
                    let childrenConfig = { config with size = childrenSize; children = attrs.children }

                    [ createWithDepth (depth + 1) childrenConfig [
                        Canvas.top childrenOffset.top
                        Canvas.left childrenOffset.left
                    ] ]

        [ Canvas.make noOffset size [
            Canvas.background (Backgrounds.next())
            Canvas.children [ topBar; rectangleView childrenViews ]
        ] ]

    let branchView depth (branch: Branch<DiskItem>) (config: Config): IView list =
        let size = config.size
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

        let createBranch tree offset size =
            Canvas.make offset size [
                Canvas.children (createTree depth tree { config with size = size })
            ]

        [ createBranch branch.left { top = 0.0; left = 0.0 } leftSize
          createBranch branch.right rightOffset rightSize ]

    let private minSize = { width = 75.0; height = 25.0 }

    let private emptyView config =
        let childSize = { width = config.size.width - leafPadding * 2.0
                          height = config.size.height - leafPadding * 2.0 }
        let childOffset = { top = leafPadding
                            left = leafPadding }

        Canvas.make noOffset config.size [
            Canvas.background (Backgrounds.next())
            Canvas.children [
                Canvas.make childOffset childSize [
                    Canvas.background leafRectangleColor
                ]
            ]
        ]

    let private createTree (depth: int) (tree: TreeNode<DiskItem>) (config: Config): IView list =
        if config.size.width >= minSize.width && config.size.height >= minSize.height
            then
                match tree with
                | LeafNode leaf -> leafView depth leaf config
                | BranchNode branch -> branchView depth branch config
            else
                [ emptyView config ]

    let private toLeaf diskItem =
        diskItem
        |> DiskItem.sizeInBytes
        |> Option.map (fun size -> { data = diskItem; weight = size })

    let createWithDepth depth config attrs: IView =
        let leaves =
            config.children
            |> List.choose toLeaf
            |> List.filter (fun leaf -> leaf.weight > int64 0)

        let childViews =
            leaves
            |> BalancedTree.create
            |> Option.map BalancedTree.root
            |> Option.map (fun root -> createTree depth root config)
            |> Option.defaultValue []

        let defaults = [ Canvas.width config.size.width
                         Canvas.height config.size.height
                         Canvas.children childViews ]

        Canvas.create (defaults @ attrs) :> IView

    let create config attrs: IView =
        Backgrounds.reset ()
        createWithDepth 0 config attrs
