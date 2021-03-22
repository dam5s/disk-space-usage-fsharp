﻿module DiskSpaceUsage.MainUI

open Elmish
open System.IO
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Components

open Icons
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
    | FinishLoading of DiskItem
    | CloseFolder
    | NavigateToItem of DiskItem
    | NavigateBack

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
        let! usage = DiskItem.loadAsync path
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
    Grid.create [
        Grid.columnDefinitions "50, *, 50"
        Grid.rowDefinitions "80, 50, 50, *, 80"
        Grid.children [
            TextBlock.create [
                Grid.row 1
                Grid.column 1
                TextBlock.verticalAlignment VerticalAlignment.Bottom
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 24.0
                TextBlock.text "No folder selected"
            ]
            Button.create [
                Grid.row 2
                Grid.column 1
                Button.height 50.0
                Button.onClick (fun _ -> dispatch OpenFolderSelectDialog)
                Button.content "Select Folder"
            ]
        ]
    ]

let private loadingView folderPath dispatch =
    Grid.create [
        Grid.columnDefinitions "*"
        Grid.rowDefinitions "80, 50, 50, *, 80"
        Grid.children [
            TextBlock.create [
                Grid.row 1
                TextBlock.verticalAlignment VerticalAlignment.Bottom
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 48.0
                TextBlock.text "Loading..."
            ]
            TextBlock.create [
                Grid.row 2
                TextBlock.verticalAlignment VerticalAlignment.Top
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 24.0
                TextBlock.text (FolderPath.path folderPath)
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
    |> List.sortByDescending (fun c -> c.percentage |> Option.defaultValue 0.0)

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
    let view = TextBlock.create [
        Grid.row 3
        TextBlock.text "file"
    ]
    view :> IView

let private itemView (diskItem: DiskItem) dispatch =
    match diskItem.itemType with
    | File -> fileView diskItem dispatch
    | Folder attrs -> folderView diskItem attrs.children dispatch

let private topDiskItem nav =
    nav.history
    |> List.tryHead
    |> Option.defaultValue nav.root

let private backButtonView dispatch =
    let view = Button.create [
        Grid.row 0

        Button.verticalAlignment VerticalAlignment.Center
        Button.horizontalAlignment HorizontalAlignment.Left
        Button.margin 20.0
        Button.content Icons.arrowLeftCircle
        Button.classes ["icon"]
        Button.onClick (fun _ -> dispatch NavigateBack)
    ]
    view :> IView

let private emptyView =
    let view = TextBlock.create [ Grid.row 0 ]
    view :> IView

let private loadedView (nav: Navigation) dispatch =
    let topItem = topDiskItem nav
    let sizeText = SizeView.text topItem.size

    let backButton =
        if List.isEmpty nav.history
        then emptyView
        else backButtonView dispatch

    Grid.create [
        Grid.columnDefinitions "*"
        Grid.rowDefinitions "80, 50, 50, *, 80"
        Grid.children [
            Button.create [
                Grid.row 0

                Button.verticalAlignment VerticalAlignment.Center
                Button.horizontalAlignment HorizontalAlignment.Right
                Button.margin 20.0
                Button.content Icons.closeCircle
                Button.classes ["icon"]
                Button.onClick (fun _ -> dispatch CloseFolder)
            ]
            TextBlock.create [
                Grid.row 1

                TextBlock.verticalAlignment VerticalAlignment.Bottom
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 48.0
                TextBlock.text sizeText
            ]
            TextBlock.create [
                Grid.row 2

                TextBlock.verticalAlignment VerticalAlignment.Top
                TextBlock.textAlignment TextAlignment.Center
                TextBlock.fontSize 24.0
                TextBlock.text topItem.name
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
