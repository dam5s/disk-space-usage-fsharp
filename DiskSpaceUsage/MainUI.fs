module DiskSpaceUsage.MainUI

open Elmish
open System.IO
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Components

open Time
open Icons
open Widgets
open FolderPath
open DiskItem
open SizeView

type Navigation =
    { root: DiskItem
      history: DiskItem list }

type AsyncDiskItem =
    | NotLoaded
    | Loading of FolderPath
    | Loaded of Navigation

type Model =
    { window: Window
      rootDiskItem : AsyncDiskItem }

let init window _ =
    { window = window
      rootDiskItem = NotLoaded }, Cmd.none

type Msg =
    | OpenFolderSelectDialog
    | SelectFolder of FolderPath
    | NowLoading of FolderPath
    | FinishLoading of DiskItem
    | CloseFolder
    | NavigateToItem of DiskItem
    | NavigateBack

module private Subscriptions =
    let mutable private dispatch = fun _ -> ()
    let mutable private lastNotify = Posix.now()

    let registerDispatch d =
        dispatch <- d

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

type GraphRow =
    { item: DiskItem
      percentage: float option
      name: string
      size: string }

module private GraphRow =
    let fileName (fullPath: string) =
        fullPath.Split(Path.DirectorySeparatorChar)
        |> Array.tryLast
        |> Option.defaultValue fullPath

    let create (parent: DiskItem) (diskItem: DiskItem) =
        let bytes = DiskItem.sizeInBytes diskItem
        let parentBytes = DiskItem.sizeInBytes parent
        let percentage =
            (bytes, parentBytes)
            ||> Option.map2 (fun b p -> float b / float p)

        { item = diskItem
          percentage = percentage
          name = diskItem.name
          size = SizeView.text diskItem.size }

let private equalSize count =
    "*"
    |> List.replicate count
    |> String.concat ","

let private sortedRows diskItem children dispatch =
    children
    |> List.map (GraphRow.create diskItem)
    |> List.sortByDescending (fun c -> c.percentage |> Option.defaultValue -1.0)

let private rowView (row: GraphRow) (dispatch: Dispatch<Msg>) =
    let rowPercentage =
        row.percentage
        |> Option.defaultValue 0.0

    let colSpan =
        100.0 * rowPercentage
        |> int
        |> max 1
        |> min 100

    let barColor =
        if rowPercentage > 0.0
        then "#339999ff"
        else "#00000000"

    let content =
        Grid.create [
            Grid.rowDefinitions "*"
            Grid.columnDefinitions (equalSize 100)
            Grid.height 30.0
            Grid.onDoubleTapped (fun _ -> NavigateToItem row.item |> dispatch)
            Grid.background "#282828"
            Grid.children [
                DockPanel.create [
                    Grid.row 0
                    Grid.column 0
                    Grid.columnSpan colSpan
                    DockPanel.background barColor
                ]
                TextBlock.create [
                    Grid.row 0
                    Grid.column 1
                    Grid.columnSpan 98
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.textAlignment TextAlignment.Left
                    TextBlock.text row.name
                ]
                TextBlock.create [
                    Grid.row 0
                    Grid.column 1
                    Grid.columnSpan 98
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.textAlignment TextAlignment.Right
                    TextBlock.text row.size
                ]
            ]
        ]

    Border.create [
        Border.borderBrush "#ffffff"
        Border.borderThickness (0.0, 0.0, 0.0, 1.0)
        Border.child content
    ]

let private folderView diskItem children dispatch =
    let view = ListBox.create [
        Grid.row 3

        ListBox.dataItems (sortedRows diskItem children dispatch)
        ListBox.itemTemplate (DataTemplateView<GraphRow>.create(fun row -> rowView row dispatch))
    ]
    view :> IView

let private fileView diskItem dispatch =
    TextBlock.create [
        Grid.row 3
        TextBlock.text "file"
    ] :> IView

let private itemView (diskItem: DiskItem) dispatch =
    match diskItem.itemType with
    | File -> fileView diskItem dispatch
    | Folder attrs -> folderView diskItem attrs.children dispatch

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

let private loadedView (nav: Navigation) dispatch =
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
            itemView topItem dispatch
        ]
    ]

let view (model: Model) (dispatch: Dispatch<Msg>) =
    match model.rootDiskItem with
    | NotLoaded -> notLoadedView dispatch
    | Loading path -> loadingView path dispatch
    | Loaded nav -> loadedView nav dispatch
