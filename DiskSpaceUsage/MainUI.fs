module DiskSpaceUsage.MainUI

open Elmish
open System.IO
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Components

open Icons
open FolderPath
open FolderUsage
open FolderSizeView

type AsyncFolderUsage =
    | NotLoaded
    | Loading of FolderPath
    | Loaded of FolderUsage

type Model =
    { window: Window
      folderUsage : AsyncFolderUsage }

let init window _ =
    { window = window
      folderUsage = NotLoaded }, Cmd.none

type Msg =
    | OpenFolderSelectDialog
    | SelectFolder of FolderPath
    | FinishLoading of FolderUsage
    | CloseFolder

let private selectFolderAsync window =
    async {
        let dialog = OpenFolderDialog ()
        dialog.Title <- "Select Folder"

        let! folderPath = dialog.ShowAsync(window) |> Async.AwaitTask

        let msg =
            folderPath
            |> FolderPath.create
            |> Option.map SelectFolder
            |> Option.defaultValue CloseFolder

        return msg
    }

let private loadFolderUsageAsync path =
    async {
        let! usage = FolderUsage.loadAsync path
        return FinishLoading usage
    }

let private asyncCmd = Cmd.OfAsync.result

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | OpenFolderSelectDialog ->
        model, asyncCmd (selectFolderAsync model.window)
    | SelectFolder path ->
        { model with folderUsage = Loading path }, asyncCmd (loadFolderUsageAsync path)
    | FinishLoading usage ->
        { model with folderUsage = Loaded usage }, Cmd.none
    | CloseFolder ->
        { model with folderUsage = NotLoaded }, Cmd.none

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
    { percentage: float
      name: string
      size: string }

module private GraphRow =
    let fileName (fullPath: string) =
        fullPath.Split(Path.DirectorySeparatorChar)
        |> Array.tryLast
        |> Option.defaultValue fullPath

    let private create name (Bytes size) (Bytes parentSize) =
        { percentage = float size / float parentSize
          name = name
          size = FolderSizeView.text (Bytes size) }

    let ofFolder (parent: FolderUsage) (folder: FolderUsage) =
        create
            (folder.path |> FolderPath.path |> fileName)
            folder.size
            parent.size

    let ofFile (parent: FolderUsage) (file: FileUsage) =
        create
            (file.path |> FilePath.path |> fileName)
            file.size
            parent.size

let private equalSize count =
    "*"
    |> List.replicate count
    |> String.concat ","

let private sortedRows parent =
    let fileCells = parent.subFiles |> List.map (GraphRow.ofFile parent)
    let folderCells = parent.subFolders |> List.map (GraphRow.ofFolder parent)

    fileCells @ folderCells
    |> List.sortBy (fun c -> - c.percentage)

let private rowView (row: GraphRow) =
    let colSpan = 100.0 * row.percentage
                  |> int
                  |> max 1
                  |> min 100
    let content =
        Grid.create [
            Grid.rowDefinitions "*"
            Grid.columnDefinitions (equalSize 100)
            Grid.height 30.0
            Grid.children [
                DockPanel.create [
                    Grid.row 0
                    Grid.column 0
                    Grid.columnSpan colSpan
                    DockPanel.background "#339999ff"
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

let private loadedView folder dispatch =
    let fullPath = FolderPath.path folder.path
    let sizeText = FolderSizeView.text folder.size

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
                TextBlock.text fullPath
            ]
            ItemsControl.create [
                Grid.row 3

                ItemsControl.dataItems (sortedRows folder)
                ItemsControl.itemTemplate (DataTemplateView<GraphRow>.create(rowView))
            ]
        ]
    ]

let view (model: Model) (dispatch: Dispatch<Msg>) =
    match model.folderUsage with
    | NotLoaded -> notLoadedView dispatch
    | Loading path -> loadingView path dispatch
    | Loaded usage -> loadedView usage dispatch
