module DiskSpaceUsage.MainUI

open Avalonia
open Elmish
open Avalonia.Layout
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Debounce
open Icons
open Styles
open FolderPath
open DiskItem
open TreeMapView
open SizeView

type AsyncDiskItem =
    | NotLoaded
    | Loading of FolderPath
    | Loaded of DiskItemNavigation

type Model =
    { window: Window
      windowBounds: Rect
      asyncDiskItem : AsyncDiskItem }

let init window _ =
    { window = window
      windowBounds = window.Bounds
      asyncDiskItem = NotLoaded }, Cmd.none

type Msg =
    | OpenFolderSelectDialog
    | SelectFolder of FolderPath
    | NowLoading of FolderPath
    | FinishLoading of DiskItem
    | CloseFolder
    | NavigateTo of DiskItemNavigation
    | NavigateUp
    | UpdateWindowBounds of Rect

[<RequireQualifiedAccess>]
module private Navigate =
    let mutable private state = Debounce.init 200

    let private debounce f =
        state <- Debounce.invoke f state

    let toItem nav dispatch =
        fun _ -> dispatch (NavigateTo nav)
        |> debounce

    let up dispatch =
        fun _ -> dispatch NavigateUp
        |> debounce

module Subscriptions =
    let mutable private dispatch = fun _ -> ()
    let mutable private notifyLoadingDebounce = Debounce.init 100

    let registerDispatch d =
        dispatch <- d

    let windowBoundsChanged newBounds =
        dispatch (UpdateWindowBounds newBounds)

    let notifyLoading folderPath =
        let f = fun _ -> dispatch (NowLoading folderPath)
        notifyLoadingDebounce <- Debounce.invoke f notifyLoadingDebounce

let subscribe model: Cmd<Msg> =
    Cmd.ofSub Subscriptions.registerDispatch

let private selectFolderAsync window =
    let dialog = OpenFolderDialog ()
    dialog.Title <- "Select Folder"

    let dialogTask = dialog.ShowAsync(window)

    async {
        let! folderPath = dialogTask |> Async.AwaitTask

        let msg =
            folderPath
            |> FolderPath.create
            |> Option.map SelectFolder
            |> Option.defaultValue CloseFolder

        return msg
    }

let private loadFolderUsageAsync path =
    async {
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()

        let! usage = DiskItem.loadAsync Subscriptions.notifyLoading path
        
        timer.Stop()
        printfn $"Finished loading in %d{timer.ElapsedMilliseconds}ms"

        return FinishLoading usage
    }

let private asyncCmd = Cmd.OfAsync.result

let private navigateUp (model: Model) =
    match model.asyncDiskItem with
    | Loaded nav ->
        match nav.parent with
        | Some parent -> { model with asyncDiskItem = Loaded parent }
        | None -> model
    | _ -> model

let private navigateToItem nav (model: Model) =
    { model with asyncDiskItem = Loaded nav }

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | OpenFolderSelectDialog ->
        model, asyncCmd (selectFolderAsync model.window)
    | SelectFolder path ->
        { model with asyncDiskItem = Loading path }, asyncCmd (loadFolderUsageAsync path)
    | NowLoading path ->
        { model with asyncDiskItem = Loading path }, Cmd.none
    | FinishLoading diskItem ->
        let loadedItem = Loaded { diskItem = diskItem; parent = None }
        { model with asyncDiskItem = loadedItem }, Cmd.none
    | CloseFolder ->
        { model with asyncDiskItem = NotLoaded }, Cmd.none
    | NavigateTo diskItem ->
        navigateToItem diskItem model, Cmd.none
    | NavigateUp ->
        navigateUp model, Cmd.none
    | UpdateWindowBounds newBounds ->
        { model with windowBounds = newBounds }, Cmd.none

let private notLoadedView dispatch =
    Grid.main [
        Grid.children [
            TextBlock.title "Disk Space Usage" [
                Grid.row 0
            ]
            TextBlock.subTitle "No folder selected" [
                Grid.row 1
            ]
            Button.create [
                Grid.row 2
                Button.onClick (fun _ -> dispatch OpenFolderSelectDialog)
                Button.content "Select Folder"
                Button.horizontalAlignment HorizontalAlignment.Center
                Button.maxWidth 250.0
                Button.margin (50.0, 0.0)
            ]
        ]
    ]

let private loadingView folderPath dispatch =
    Grid.main [
        Grid.children [
            TextBlock.title "Loading..." [
                Grid.row 0
            ]
            TextBlock.subTitle (FolderPath.path folderPath) [
                Grid.row 1
            ]
        ]
    ]

let private folderView model (nav: DiskItemNavigation) (children: DiskItem list) dispatch =
    let windowBounds = model.window.Bounds

    let treeSize =
        { width = windowBounds.Width - 100.0
          height = Grid.resizableRowHeight windowBounds.Height |> double }

    let childNav c = { diskItem = c; parent = Some nav }

    let treeViewConfig: TreeMapView.Config =
        { children = children |> List.map childNav
          size = treeSize
          onItemSelected = fun nav -> Navigate.toItem nav dispatch }

    TreeMapView.create treeViewConfig [
        Grid.row 3
    ]

let private fileView diskItem dispatch =
    TextBlock.create [
        Grid.row 3
        TextBlock.text "file"
    ] :> IView

let private itemView model (nav: DiskItemNavigation) dispatch =
    match nav.diskItem.itemType with
    | File -> fileView nav dispatch
    | Folder attrs -> folderView model nav attrs.children dispatch

let private upButtonView parent dispatch attrs =
    let (enabled, color) =
        parent
        |> Option.map (fun _ -> true, Enabled)
        |> Option.defaultValue (false, Disabled)

    let defaults = [
        Button.verticalAlignment VerticalAlignment.Center
        Button.onClick (fun _ -> Navigate.up dispatch)
        Button.isEnabled enabled
    ]
    Button.icon color ArrowLeftCircle (defaults @ attrs)

let rec navItemParents firstParent =
    match firstParent with
    | Some parent -> (navItemParents parent.parent) @ [ parent ]
    | None -> []

let private breadcrumbsView dispatch nav =
    let parentsButtons =
        navItemParents nav.parent
        |> List.map (fun p ->
            Button.create [
                Button.content p.diskItem.name
                Button.onTapped ((fun _ -> dispatch (NavigateTo p)), OnChangeOf p)
            ] :> IView
        )

    let children = parentsButtons @ [
        Button.create [
            Button.content nav.diskItem.name
            Button.isEnabled false
        ]
    ]

    StackPanel.create [
        DockPanel.dock Dock.Right
        StackPanel.horizontalAlignment HorizontalAlignment.Center
        StackPanel.orientation Orientation.Horizontal
        StackPanel.children children
    ]

let private emptyView =
    TextBlock.create [
        DockPanel.dock Dock.Right
    ] :> IView

let navBar nav dispatch =
    let upButton =
        upButtonView nav.parent dispatch [
            DockPanel.dock Dock.Left
        ]

    let breadCrumbs =
        breadcrumbsView dispatch nav

    let closeButton =
        Button.navBarIcon Enabled CloseCircle [
            DockPanel.dock Dock.Right
            Button.horizontalAlignment HorizontalAlignment.Right
            Button.onClick (fun _ -> dispatch CloseFolder)
        ]

    DockPanel.create [
        Grid.row 2
        DockPanel.children [
            upButton
            closeButton
            breadCrumbs
        ]
    ]

let private loadedView model (nav: DiskItemNavigation) dispatch =
    let sizeText = SizeView.text nav.diskItem.size

    Grid.main [
        Grid.children [
            TextBlock.title sizeText [
                Grid.row 0
            ]
            TextBlock.subTitle nav.diskItem.name [
                Grid.row 1
            ]
            navBar nav dispatch
            itemView model nav dispatch
        ]
    ]

let view (model: Model) (dispatch: Dispatch<Msg>) =
    match model.asyncDiskItem with
    | NotLoaded -> notLoadedView dispatch
    | Loading path -> loadingView path dispatch
    | Loaded nav -> loadedView model nav dispatch
