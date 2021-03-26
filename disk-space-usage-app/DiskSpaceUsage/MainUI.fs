module DiskSpaceUsage.MainUI

open Avalonia
open Elmish
open Avalonia.Layout
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Time
open Icons
open Styles
open FolderPath
open DiskItem
open SizeView
open TreeMapView

type Navigation =
    { root: DiskItem
      history: DiskItem list }

type AsyncDiskItem =
    | NotLoaded
    | Loading of FolderPath
    | Loaded of Navigation

type Model =
    { window: Window
      windowBounds: Rect
      rootDiskItem : AsyncDiskItem }

let init window _ =
    { window = window
      windowBounds = window.Bounds
      rootDiskItem = NotLoaded }, Cmd.none

type Msg =
    | OpenFolderSelectDialog
    | SelectFolder of FolderPath
    | NowLoading of FolderPath
    | FinishLoading of DiskItem
    | CloseFolder
    | NavigateToItem of DiskItem
    | NavigateBack
    | UpdateWindowBounds of Rect

module Subscriptions =
    let mutable private dispatch = fun _ -> ()
    let mutable private lastNotify = Posix.now()

    let registerDispatch d =
        dispatch <- d

    let windowBoundsChanged newBounds =
        dispatch (UpdateWindowBounds newBounds)

    let notifyLoading folderPath =
        let now = Posix.now()
        let msSinceLastNotify = Posix.milliseconds now - Posix.milliseconds lastNotify

        if msSinceLastNotify > int64 100
        then
            lastNotify <- now
            dispatch (NowLoading folderPath)
        else
            ()

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
        let! usage = DiskItem.loadAsync Subscriptions.notifyLoading path
        return FinishLoading usage
    }

let private asyncCmd = Cmd.OfAsync.result

let private navigateToItem (diskItem: DiskItem) (nav: Navigation) =
    { nav with history = diskItem :: nav.history}

let private navigateBack (nav: Navigation) =
    match nav.history with
    | _ :: tail -> { nav with history = tail }
    | [] -> { nav with history = [] }

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | OpenFolderSelectDialog ->
        model, asyncCmd (selectFolderAsync model.window)
    | SelectFolder path ->
        { model with rootDiskItem = Loading path }, asyncCmd (loadFolderUsageAsync path)
    | NowLoading path ->
        { model with rootDiskItem = Loading path }, Cmd.none
    | FinishLoading diskItem ->
        let nav = { root = diskItem; history = [] }
        { model with rootDiskItem = Loaded nav }, Cmd.none
    | CloseFolder ->
        { model with rootDiskItem = NotLoaded }, Cmd.none
    | NavigateToItem diskItem ->
        match model.rootDiskItem with
        | Loaded nav -> { model with rootDiskItem = Loaded (navigateToItem diskItem nav) }, Cmd.none
        | _ -> model, Cmd.none
    | NavigateBack ->
        match model.rootDiskItem with
        | Loaded nav -> { model with rootDiskItem = Loaded (navigateBack nav) }, Cmd.none
        | _ -> model, Cmd.none
    | UpdateWindowBounds newBounds ->
        { model with windowBounds = newBounds }, Cmd.none

let private notLoadedView dispatch =
    Grid.main [
        Grid.children [
            TextBlock.subTitle "No folder selected" [
                Grid.row 1
                TextBlock.verticalAlignment VerticalAlignment.Bottom
            ]
            Button.create [
                Grid.row 2
                Button.height 50.0
                Button.onClick (fun _ -> dispatch OpenFolderSelectDialog)
                Button.content "Select Folder"
                Button.margin (50.0, 0.0)
            ]
        ]
    ]

let private loadingView folderPath dispatch =
    Grid.main [
        Grid.children [
            TextBlock.title "Loading..." [
                Grid.row 1
            ]
            TextBlock.subTitle (FolderPath.path folderPath) [
                Grid.row 2
            ]
        ]
    ]

let private folderView model diskItem children dispatch =
    let windowBounds = model.window.Bounds

    let treeSize =
        { width = windowBounds.Width - 100.0
          height = Grid.resizableRowHeight windowBounds.Height |> double }

    let treeViewConfig: TreeMapView.Config =
        { children = children
          size = treeSize
          onItemSelected = fun item -> NavigateToItem item |> dispatch }

    TreeMapView.create treeViewConfig [
        Grid.row 3
    ]

let private fileView diskItem dispatch =
    TextBlock.create [
        Grid.row 3
        TextBlock.text "file"
    ] :> IView

let private itemView model (diskItem: DiskItem) dispatch =
    match diskItem.itemType with
    | File -> fileView diskItem dispatch
    | Folder attrs -> folderView model diskItem attrs.children dispatch

let private topDiskItem nav =
    nav.history
    |> List.tryHead
    |> Option.defaultValue nav.root

let private backButtonView dispatch =
    Button.icon Icons.arrowLeftCircle [
        Grid.row 0
        Button.horizontalAlignment HorizontalAlignment.Left
        Button.onClick (fun _ -> dispatch NavigateBack)
    ]

let private emptyView =
    TextBlock.create [ Grid.row 0 ] :> IView

let private loadedView model (nav: Navigation) dispatch =
    let topItem = topDiskItem nav
    let sizeText = SizeView.text topItem.size

    let backButton =
        if List.isEmpty nav.history
        then emptyView
        else backButtonView dispatch

    Grid.main [
        Grid.children [
            Button.icon Icons.closeCircle [
                Grid.row 0
                Button.horizontalAlignment HorizontalAlignment.Right
                Button.onClick (fun _ -> dispatch CloseFolder)
            ]
            TextBlock.title sizeText [
                Grid.row 1
            ]
            TextBlock.subTitle topItem.name [
                Grid.row 2
            ]
            backButton
            itemView model topItem dispatch
        ]
    ]

let view (model: Model) (dispatch: Dispatch<Msg>) =
    match model.rootDiskItem with
    | NotLoaded -> notLoadedView dispatch
    | Loading path -> loadingView path dispatch
    | Loaded nav -> loadedView model nav dispatch
